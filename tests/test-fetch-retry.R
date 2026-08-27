#!/usr/bin/env Rscript
# ---------------------------------------------------------------
# Tests for the retry/validation wrapper around tidyquant::tq_get.
#
# Regression cover for the 2026-08-25 and 2026-08-26 pipeline failures,
# where a transient upstream hiccup made tq_get() fail *soft* and the run
# died much later with a cryptic dplyr error:
#
#   2026-08-25  Yahoo ^GSPC unreachable -> tq_get returned logical NA
#               -> "no applicable method for 'select' applied to an
#                   object of class logical"   (R/01-sp500.R)
#
#   2026-08-26  FRED dropped M1SL from the result
#               -> "object 'm1sl' not found"   (R/05-fred-api.R)
#
# These run offline: the fetcher is injected, no network required.
# ---------------------------------------------------------------

suppressPackageStartupMessages({
  library(tibble)
})

# run from the project root: Rscript tests/test-fetch-retry.R
source("R/00-utils.R")

# --- tiny test harness -----------------------------------------

failures <- 0L
checks   <- 0L

check <- function(label, expr) {
  checks <<- checks + 1L
  ok <- tryCatch(isTRUE(expr), error = function(e) {
    cat("   (error while evaluating:", conditionMessage(e), ")\n")
    FALSE
  })
  cat(if (ok) "  PASS  " else "  FAIL  ", label, "\n")
  if (!ok) failures <<- failures + 1L
  invisible(ok)
}

err_msg <- function(expr) {
  tryCatch({
    force(expr)
    NA_character_
  }, error = function(e) conditionMessage(e))
}

# --- fixtures --------------------------------------------------

# what tq_get returns for a healthy multi-series economic.data pull
fred_ok <- function(symbols) {
  do.call(rbind, lapply(symbols, function(s) {
    tibble::tibble(
      symbol = s,
      date   = as.Date("2026-01-01") + 0:5,
      price  = as.numeric(1:6)
    )
  }))
}

# what tq_get returns for a healthy stock.prices pull
prices_ok <- function() {
  tibble::tibble(
    date     = as.Date("2026-08-01") + 0:9,
    open     = 1:10, high = 1:10, low = 1:10,
    close    = as.numeric(1:10),
    volume   = 1:10, adjusted = as.numeric(1:10)
  )
}

cat("\n=== tq_get_retry: soft-failure detection ===\n")

# 1. Yahoo mode: tq_get returns logical NA (the 2026-08-25 failure)
msg <- err_msg(
  tq_get_retry("^GSPC", get = "stock.prices",
               tries = 2, delay = 0,
               .fetcher = function(...) NA)
)
check("stock.prices soft-fail raises an error (not a logical NA)",
      !is.na(msg))
check("error names the symbol ^GSPC",
      grepl("\\^GSPC", msg, fixed = FALSE))
check("error mentions the attempt count",
      grepl("2 attempt", msg))

# 2. FRED mode: one series silently dropped (the 2026-08-26 failure)
msg <- err_msg(
  tq_get_retry(c("INDPRO", "M1SL", "TCU"), get = "economic.data",
               expect = c("INDPRO", "M1SL", "TCU"),
               tries = 2, delay = 0,
               .fetcher = function(...) fred_ok(c("INDPRO", "TCU")))
)
check("dropped series raises an error",
      !is.na(msg))
check("error names exactly the missing series (M1SL)",
      grepl("M1SL", msg) && !grepl("INDPRO", msg))

# 3. empty frame is a failure, not success
msg <- err_msg(
  tq_get_retry("USREC", get = "economic.data", expect = "USREC",
               tries = 2, delay = 0,
               .fetcher = function(...) fred_ok(character(0)))
)
check("empty result raises an error", !is.na(msg))

cat("\n=== tq_get_retry: retry actually retries ===\n")

# 4. transient failure then success -> must succeed, no error
attempts <- 0L
res <- tq_get_retry(
  c("INDPRO", "M1SL"), get = "economic.data",
  expect = c("INDPRO", "M1SL"),
  tries = 4, delay = 0,
  .fetcher = function(...) {
    attempts <<- attempts + 1L
    if (attempts < 3L) fred_ok("INDPRO") else fred_ok(c("INDPRO", "M1SL"))
  }
)
check("recovers from 2 transient soft-failures", is.data.frame(res))
check("made exactly 3 attempts", attempts == 3L)
check("returns all expected series",
      setequal(unique(res$symbol), c("INDPRO", "M1SL")))

# 5. hard error (thrown condition) is also retried
attempts <- 0L
res <- tq_get_retry(
  "^GSPC", get = "stock.prices", tries = 3, delay = 0,
  .fetcher = function(...) {
    attempts <<- attempts + 1L
    if (attempts < 2L) stop("cannot open the connection") else prices_ok()
  }
)
check("recovers from a thrown connection error", is.data.frame(res))
check("made exactly 2 attempts", attempts == 2L)

cat("\n=== tq_get_retry: happy path is untouched ===\n")

attempts <- 0L
res <- tq_get_retry("^GSPC", get = "stock.prices", tries = 3, delay = 0,
                    .fetcher = function(...) { attempts <<- attempts + 1L; prices_ok() })
check("returns the frame unchanged", identical(res, prices_ok()))
check("does not retry on success", attempts == 1L)

# --- summary ---------------------------------------------------

cat("\n----------------------------------------\n")
cat(sprintf("%d checks, %d failure(s)\n", checks, failures))
if (failures > 0L) quit(status = 1L)
cat("OK\n")
