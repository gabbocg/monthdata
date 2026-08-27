# ---------------------------------------------------------------
# Shared download helpers
#
# tidyquant::tq_get() fails *soft*: when a source is briefly unreachable it
# emits a warning and either returns a logical NA (get = "stock.prices") or
# silently drops the failed ticker from the result (get = "economic.data").
# Callers that assume the data arrived then blow up much later with a
# cryptic dplyr error, and the whole daily run is lost to a network blip:
#
#   2026-08-25  "no applicable method for 'select' applied to an object
#                of class logical"      <- Yahoo ^GSPC returned NA
#   2026-08-26  "object 'm1sl' not found"  <- FRED dropped M1SL
#
# tq_get_retry() closes that gap: it retries with exponential backoff, then
# fails loudly naming exactly what is missing, before any downstream code
# gets a chance to misinterpret a half-empty result.
# ---------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a

# Which symbols does a tq_get result actually contain?
# Multi-ticker pulls carry a `symbol` column; single-ticker pulls may not.
.tq_symbols_present <- function(res, expect) {
  if ("symbol" %in% names(res)) return(unique(as.character(res$symbol)))
  if (length(expect) == 1L) return(expect)  # single-ticker shape, rows exist
  character(0)
}

# Return NULL when the result is usable, or a one-line description of what
# is wrong with it.
.tq_problem <- function(res, get, expect) {

  if (inherits(res, "tq_fetch_error")) return(as.character(res))

  if (!is.data.frame(res)) {
    return(sprintf("source returned a %s instead of a data frame",
                   paste(class(res), collapse = "/")))
  }

  if (nrow(res) == 0L) return("source returned no rows")

  if (identical(get, "economic.data")) {
    if (!"price" %in% names(res)) return("result has no `price` column")
    if (!is.null(expect)) {
      missing <- setdiff(expect, .tq_symbols_present(res, expect))
      if (length(missing) > 0L) {
        return(sprintf("missing series: %s", paste(missing, collapse = ", ")))
      }
    }
  }

  if (identical(get, "stock.prices")) {
    needed  <- c("date", "close")
    missing <- setdiff(needed, names(res))
    if (length(missing) > 0L) {
      return(sprintf("result has no %s column",
                     paste(missing, collapse = "/")))
    }
  }

  NULL
}

#' Fetch via tq_get, retrying transient failures, then fail loudly.
#'
#' @param x        ticker(s), passed straight to tq_get
#' @param get      "economic.data" or "stock.prices"
#' @param expect   character vector of series that MUST be present
#'                 (economic.data only). Defaults to `x`.
#' @param tries    maximum attempts before giving up
#' @param delay    base seconds for exponential backoff (delay * 2^(n-1))
#' @param label    human-readable source name used in the error message
#' @param .fetcher injection point for tests; defaults to tidyquant::tq_get
tq_get_retry <- function(x, get, ..., expect = NULL, tries = 5, delay = 2,
                         label = NULL, .fetcher = tidyquant::tq_get) {

  if (is.null(expect) && identical(get, "economic.data")) expect <- x

  label <- label %||% if (length(x) == 1L) {
    as.character(x)
  } else {
    sprintf("%d %s series", length(x), get)
  }

  problem <- NULL

  for (attempt in seq_len(tries)) {

    res <- tryCatch(
      .fetcher(x, get = get, ...),
      error = function(e) structure(conditionMessage(e),
                                    class = "tq_fetch_error")
    )

    problem <- .tq_problem(res, get = get, expect = expect)
    if (is.null(problem)) return(res)

    if (attempt < tries) {
      wait <- delay * 2^(attempt - 1L)
      cat(sprintf("   retry %d/%d for %s in %gs (%s)\n",
                  attempt, tries - 1L, label, wait, problem))
      Sys.sleep(wait)
    }
  }

  stop(sprintf("Download failed for %s after %d attempts: %s",
               label, tries, problem), call. = FALSE)
}

#' Replace quantmod's FRED downloader with one that uses the system curl
#' binary. quantmod fetches through a `curl::curl()` connection, which fails
#' against FRED's HTTP/2 server with "HTTP/2 stream not closed cleanly:
#' INTERNAL_ERROR" on some libcurl builds. Shelling out avoids that codepath
#' and lets us ask curl for real retries.
#'
#' Failed symbols are still dropped with a warning (quantmod's contract) --
#' tq_get_retry() above is what turns that into a loud, named error.
install_fred_downloader <- function() {

  utils::assignInNamespace("getSymbols.FRED", function(Symbols, env,
                                                       return.class = "xts",
                                                       ...) {

    args        <- list(...)
    verbose     <- isTRUE(args$verbose)
    auto.assign <- if (is.null(args$auto.assign)) TRUE else isTRUE(args$auto.assign)
    warnings    <- if (is.null(args$warnings))    TRUE else isTRUE(args$warnings)
    from        <- if (is.null(args$from))        ""   else args$from
    to          <- if (is.null(args$to))          ""   else args$to

    FRED.URL  <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id="
    returnSym <- Symbols
    noDataSym <- NULL
    fr <- NULL

    for (i in seq_along(Symbols)) {

      sym <- Symbols[[i]]

      if (verbose) cat("downloading ", sym, ".....\n\n")
      test <- try({

        tmp_file <- tempfile(fileext = ".csv")
        # --fail            : treat 4xx/5xx as an error rather than saving the
        #                     HTML error page for read.csv to parse as garbage
        # --retry-all-errors: plain --retry covers only timeouts and 5xx, so it
        #                     misses the mid-stream HTTP/2 INTERNAL_ERROR abort
        #                     this override exists to work around (curl exit 92)
        utils::download.file(
          paste0(FRED.URL, sym), tmp_file,
          method = "curl", mode = "wb", quiet = TRUE,
          extra = paste(
            "--silent --show-error --location --fail",
            "--retry 5 --retry-delay 2 --retry-all-errors",
            "--connect-timeout 20 --max-time 120"
          )
        )

        raw <- utils::read.csv(tmp_file, na.strings = ".")
        unlink(tmp_file)

        fr <- xts::xts(
          as.matrix(raw[, -1]),
          as.Date(raw[, 1], origin = "1970-01-01"),
          src = "FRED", updated = Sys.time()
        )

        dim(fr) <- c(NROW(fr), 1)
        colnames(fr) <- toupper(sym)
        fr <- fr[paste(from, to, sep = "/")]
        fr <- quantmod:::convert.time.series(fr = fr, return.class = return.class)

        if (auto.assign) assign(toupper(gsub("\\^", "", sym)), fr, env)

      }, silent = TRUE)

      if (inherits(test, "try-error")) {

        msg <- paste0("Unable to import ", dQuote(sym), ".\n",
                      attr(test, "condition")$message)
        if (isTRUE(warnings)) warning(msg, call. = FALSE, immediate. = TRUE)
        noDataSym <- c(noDataSym, sym)

      }

    }

    if (auto.assign) return(setdiff(returnSym, noDataSym))
    fr

  }, ns = "quantmod")

  invisible(TRUE)
}

#' Hard assertion that a data frame carries the columns downstream code needs.
#' Turns a would-be "object 'foo' not found" into a named, actionable error.
require_cols <- function(df, cols, what) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0L) {
    stop(sprintf("%s is missing expected column(s): %s",
                 what, paste(missing, collapse = ", ")), call. = FALSE)
  }
  invisible(df)
}
