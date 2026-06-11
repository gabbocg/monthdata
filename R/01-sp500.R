# ---------------------------------------------------------------
# S&P 500: Daily prices, monthly aggregation, realized volatility
# Source: Yahoo Finance via tidyquant
# ---------------------------------------------------------------

download_sp500 <- function(from = "1927-12-01", to = Sys.Date()) {
  
  cat(">> Downloading S&P 500 daily data from Yahoo Finance...\n")
  
  sp500_daily <- tidyquant::tq_get(
    x    = "^GSPC",
    get  = "stock.prices",
    from = from,
    to   = as.character(to)
  )
  
  # daily squared log-returns
  sqret_daily <- sp500_daily |>
    dplyr::select(date, close) |>
    dplyr::mutate(
      log_ret = log(close / dplyr::lag(close, 1)),
      month   = as.numeric(strftime(date, "%m")),
      year    = lubridate::year(date)
    ) |>
    tidyr::drop_na() |>
    dplyr::mutate(
      ret2   = log_ret^2,
      yyyymm = as.numeric(paste0(year, sprintf("%02d", month)))
    ) |>
    dplyr::select(yyyymm, year, month, ret2)
  
  # monthly sum of squared returns
  sqret_monthly <- sp500_daily |>
    dplyr::select(date, close) |>
    dplyr::mutate(
      log_ret = log(close / dplyr::lag(close, 1)),
      month   = as.numeric(strftime(date, "%m")),
      year    = lubridate::year(date)
    ) |>
    tidyr::drop_na() |>
    dplyr::group_by(year, month) |>
    dplyr::summarise(
      ret2   = sum(log_ret^2),
      .groups = "drop"
    ) |>
    dplyr::mutate(yyyymm = as.numeric(paste0(year, sprintf("%02d", month)))) |>
    dplyr::select(yyyymm, year, month, ret2)
  
  # monthly realized volatility
  rv_stocks <- sp500_daily |>
    dplyr::select(date, close) |>
    dplyr::mutate(
      log_ret = log(close / dplyr::lag(close, 1)),
      month   = as.numeric(strftime(date, "%m")),
      year    = lubridate::year(date)
    ) |>
    tidyr::drop_na() |>
    dplyr::group_by(year, month) |>
    dplyr::summarise(
      rv = sqrt(sum(log_ret^2)),
      .groups = "drop"
    ) |>
    dplyr::mutate(yyyymm = as.numeric(paste0(year, sprintf("%02d", month)))) |>
    dplyr::select(yyyymm, year, month, rv)
  
  # monthly close and volume
  sp500_monthly <- sp500_daily |>
    # Yahoo sometimes returns a pre-market stub row with NA close/volume for the
    # current trading day; drop it so last(close)/sum(volume) don't pick up NA.
    dplyr::filter(!is.na(close)) |>
    dplyr::mutate(
      month = lubridate::month(date),
      year  = lubridate::year(date)
    ) |>
    dplyr::group_by(year, month) |>
    dplyr::summarise(
      close  = dplyr::last(close),
      volume = sum(volume),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      date = as.numeric(paste0(year, sprintf("%02d", month)))
    ) |>
    dplyr::select(date, close, volume)
  
  list(
    sp500_daily    = sp500_daily,
    sqret_daily    = sqret_daily,
    sqret_monthly  = sqret_monthly,
    rv_stocks      = rv_stocks,
    sp500_monthly  = sp500_monthly
  )
  
}
