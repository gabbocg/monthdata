# ---------------------------------------------------------------
# Cochrane-Piazzesi (2005) Bond Risk Premium Factor
# Source: Computed from FRED yield curve data
#   - GS1, GS2, GS5, GS10 (Treasury constant maturity rates)
#   - TB3MS (3-month T-bill)
# Uses the CP (2005) forward rate regression coefficients
# ---------------------------------------------------------------

download_cochrane_piazzesi <- function(from = "1952-01-01", to = Sys.Date()) {
  
  cat(">> Computing Cochrane-Piazzesi factor from FRED yields...\n")
  
  tickers <- c("TB3MS", "GS1", "GS2", "GS5", "GS10")
  
  yields_raw <- tq_get_retry(
    tickers,
    get    = "economic.data",
    from   = from,
    to     = as.character(to),
    expect = tickers,
    label  = "FRED yield curve series"
  ) |>
    tidyr::pivot_wider(names_from = symbol, values_from = price) |>
    janitor::clean_names() |>
    require_cols(tolower(tickers), what = "FRED yield curve data") |>
    tidyr::drop_na()
  
  # Cochrane-Piazzesi (2005) approach:
  # CP factor = gamma_0 + gamma_1*y1 + gamma_2*f2 + gamma_3*f3 + gamma_4*f4 + gamma_5*f5
  # where f_n are forward rates derived from yields
  # We approximate forward rates from available yields:
  #   f(1) ~ tb3ms/gs1 proxy
  #   f(2) ~ 2*gs2 - gs1
  #   f(3) ~ (5*gs5 - 2*gs2) / 3  (interpolated)
  #   f(4) ~ (10*gs10 - 5*gs5) / 5
  #
  # CP (2005) Table 3 coefficients (approximate):
  # gamma = (-1.69, 0.81, -0.41, -0.18, 0.31, 0.80)
  
  cp_data <- yields_raw |>
    dplyr::mutate(
      y1 = gs1 / 100,
      f2 = (2 * gs2 - gs1) / 100,
      f3 = ((5 * gs5 - 2 * gs2) / 3) / 100,
      f4 = f3,  # approximation given limited maturities
      f5 = ((10 * gs10 - 5 * gs5) / 5) / 100,
      # CP factor using Table 3 coefficients
      cp = -1.69 + 0.81 * y1 + (-0.41) * f2 + (-0.18) * f3 + 0.31 * f4 + 0.80 * f5,
      month  = lubridate::month(date),
      year   = lubridate::year(date),
      yyyymm = as.numeric(paste0(year, sprintf("%02d", month)))
    ) |>
    dplyr::select(yyyymm, year, month, cp)
  
  cp_data
  
}
