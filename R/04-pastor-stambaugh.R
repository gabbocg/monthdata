# ---------------------------------------------------------------
# Pastor-Stambaugh Aggregate Liquidity Factor
# Source: Lubos Pastor, Chicago Booth
# ---------------------------------------------------------------

download_pastor_stambaugh <- function() {
  
  cat(">> Downloading Pastor-Stambaugh liquidity factor...\n")
  
  urls <- c(
    "https://faculty.chicagobooth.edu/-/media/faculty/lubos-pastor/data/liq_data_1962_2025.txt",
    "https://faculty.chicagobooth.edu/-/media/faculty/lubos-pastor/data/liq_data_1962_2024.txt"
  )
  
  tmp <- tempfile(fileext = ".txt")

  old_timeout <- getOption("timeout")
  options(timeout = max(300, old_timeout))
  on.exit(options(timeout = old_timeout), add = TRUE)

  ok <- FALSE

  ua <- paste0(
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
    "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36"
  )

  curl_extra <- c(
    "--silent", "--show-error", "--location",
    "--max-time", "60",
    "--user-agent", shQuote(ua)
  )

  for (url in urls) {

    for (attempt in 1:3) {

      if (file.exists(tmp)) unlink(tmp)

      res <- try(
        utils::download.file(
          url, tmp,
          mode   = "wb",
          quiet  = TRUE,
          method = "curl",
          extra  = curl_extra
        ),
        silent = TRUE
      )

      if (!inherits(res, "try-error") &&
            file.exists(tmp) &&
            file.info(tmp)$size > 0) {

        ok <- TRUE
        break

      }

      Sys.sleep(2 * attempt)

    }

    if (ok) break

  }

  if (!ok) {
    stop("Failed to download Pastor-Stambaugh liquidity data after retries.")
  }

  # read skipping comment lines (start with %)
  lines <- readLines(tmp)
  unlink(tmp)

  data_lines <- lines[!grepl("^%", lines) & trimws(lines) != ""]
  df <- utils::read.table(text = data_lines, header = FALSE, fill = TRUE)
  colnames(df) <- c("yyyymm", "agg_liq", "innov_liq", "traded_liq")
  
  pastor <- df |>
    tibble::as_tibble() |>
    dplyr::mutate(
      yyyymm = as.numeric(yyyymm),
      ps     = as.numeric(innov_liq),
      year   = as.numeric(stringr::str_sub(yyyymm, 1, 4)),
      month  = as.numeric(stringr::str_sub(yyyymm, -2))
    ) |>
    dplyr::mutate(ps = dplyr::if_else(ps == -99, NA_real_, ps)) |>
    dplyr::select(yyyymm, year, month, ps)
  
  pastor
  
}
