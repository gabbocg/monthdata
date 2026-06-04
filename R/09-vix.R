# ---------------------------------------------------------------
# VIX (CBOE Volatility Index)
# Source: FRED (VIXCLS)
# ---------------------------------------------------------------

download_vix <- function(from = "1990-01-01", to = Sys.Date()) {

  cat(">> Downloading VIX from FRED...\n")

  vix_daily <- tryCatch(
    tidyquant::tq_get(
      "VIXCLS",
      get  = "economic.data",
      from = from,
      to   = as.character(to)
    ),
    error = function(e) NULL
  )

  # fredgraph.csv occasionally returns 504 for individual series while the
  # fredgraph.xls endpoint (same data, Excel format) keeps working. Fall back.
  csv_failed <- !is.data.frame(vix_daily) ||
    nrow(vix_daily) == 0 ||
    !"price" %in% names(vix_daily) ||
    all(is.na(vix_daily$price))

  if (csv_failed) {

    cat("   fredgraph.csv unavailable; falling back to fredgraph.xls...\n")
    
    tmp <- tempfile(fileext = ".xlsx")
    download.file(
      "https://fred.stlouisfed.org/graph/fredgraph.xls?id=VIXCLS",
      tmp, mode = "wb", method = "curl", quiet = TRUE
    )

    vix_daily <- readxl::read_xlsx(
      tmp,
      sheet     = "Daily, Close",
      col_types = c("date", "numeric")
    ) |>
      dplyr::rename(date = "observation_date", price = "VIXCLS") |>
      dplyr::mutate(date = as.Date(date)) |>
      dplyr::filter(date >= as.Date(from), date <= as.Date(to))

  }

  if (!is.data.frame(vix_daily) || nrow(vix_daily) == 0 ||
        all(is.na(vix_daily$price))) {

    stop("download_vix: both fredgraph.csv and fredgraph.xls returned no data")
    
  }

  # aggregate to monthly (end-of-month close)
  vix <- vix_daily |>
    tidyr::drop_na(price) |>
    dplyr::mutate(
      month = lubridate::month(date),
      year  = lubridate::year(date)
    ) |>
    dplyr::group_by(year, month) |>
    dplyr::summarise(vix = dplyr::last(price), .groups = "drop") |>
    dplyr::mutate(
      yyyymm = as.numeric(paste0(year, sprintf("%02d", month)))
    ) |>
    dplyr::select(yyyymm, year, month, vix)
  
  vix
  
}
