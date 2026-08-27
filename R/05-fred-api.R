# ---------------------------------------------------------------
# Macroeconomic variables from FRED API
# Source: Federal Reserve Economic Data via tidyquant
# ---------------------------------------------------------------

download_fred_api <- function(from = "1959-01-01", to = Sys.Date()) {
  
  cat(">> Downloading macroeconomic data from FRED...\n")
  
  tickers <- c("INDPRO", "M1SL", "TCU", "PAYEMS", "UMCSENT", "HOUST")
  
  fred_raw <- tq_get_retry(
    tickers,
    get    = "economic.data",
    from   = from,
    to     = as.character(to),
    expect = tickers,
    label  = "FRED macro series"
  ) |>
    tidyr::pivot_wider(names_from = symbol, values_from = price) |>
    janitor::clean_names() |>
    require_cols(tolower(tickers), what = "FRED macro data")
  
  fred_api <- fred_raw |>
    dplyr::mutate(
      ipm  = log(indpro / dplyr::lag(indpro, 1)),
      ipa  = log(indpro / dplyr::lag(indpro, 12)),
      m1m  = log(m1sl / dplyr::lag(m1sl, 1)),
      m1a  = log(m1sl / dplyr::lag(m1sl, 12)),
      cap  = log(tcu / dplyr::lag(tcu, 1)),
      empl = (payems - dplyr::lag(payems, 1)) / dplyr::lag(payems, 1),
      sent = (umcsent - dplyr::lag(umcsent, 1)) / dplyr::lag(umcsent, 1),
      hs   = (houst - dplyr::lag(houst, 1)) / dplyr::lag(houst, 1)
    ) |>
    dplyr::mutate(
      month  = as.numeric(strftime(date, "%m")),
      year   = lubridate::year(date),
      yyyymm = as.numeric(paste0(year, sprintf("%02d", month)))
    ) |>
    dplyr::select(yyyymm, year, month, ipm, ipa, m1m, m1a, cap, empl, sent, hs)
  
  fred_api
  
}
