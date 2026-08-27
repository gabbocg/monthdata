# ---------------------------------------------------------------
# US Recession Indicator
# Source: FRED (USREC)
# ---------------------------------------------------------------

download_usrec <- function(from = "1925-01-01", to = Sys.Date()) {
  
  cat(">> Downloading US recession indicator from FRED...\n")
  
  usrec <- tq_get_retry(
    "USREC",
    get   = "economic.data",
    from  = from,
    to    = as.character(to),
    label = "USREC (FRED)"
  ) |>
    dplyr::rename(usrec = price) |>
    dplyr::mutate(
      month  = lubridate::month(date),
      year   = lubridate::year(date),
      yyyymm = as.numeric(paste0(year, sprintf("%02d", month)))
    ) |>
    dplyr::select(yyyymm, year, month, usrec)
  
  usrec
  
}
