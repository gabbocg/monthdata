#!/usr/bin/env Rscript
# ======================================================== #
#
#             Generate DHC Predictors Datasets
#
#                 Gabriel E. Cabrera-Guzmán
#                The University of Manchester
#
#                       Spring, 2026
#
#                https://gcabrerag.rbind.io
#
# ------------------------------ #
# email: gabriel.cabreraguzman@postgrad.manchester.ac.uk
# ======================================================== #

# Downloads all data from original sources and merges into a single
# predictor dataset. Extended to latest available observation.
#
# Data Sources:
#   - S&P 500:           Yahoo Finance (tidyquant)
#   - Goyal-Welch:       Amit Goyal's Google Drive
#   - Kenneth French:    Ken French Data Library
#   - Pastor-Stambaugh:  Chicago Booth (Lubos Pastor)
#   - Cochrane-Piazzesi: Computed from FRED yield curve
#   - FRED API:          Federal Reserve Economic Data
#   - FRED-MD:           McCracken & Ng (fredmd package)
#   - Datastream vars:   FRED/Yahoo Finance proxies
#   - Uncertainty:       FRED, nancyxu.net, policyuncertainty.com
#   - VIX:               FRED (VIXCLS)
#   - US Recession:      FRED (USREC)

# ==========================================
#                   Setup
# ------------------------------------------

# Load packages
library(tidyquant)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(readxl)
library(stringr)
library(janitor)
library(magrittr)
library(openxlsx)
library(readr)
library(TTR)
library(googledrive)

# ------------------------------------------
# Override quantmod's FRED downloader to use system curl. The default
# `curl::curl()` connection sometimes fails against FRED's HTTP/2 server
# with "HTTP/2 stream not closed cleanly" errors on macOS/libcurl. Using
# the system curl binary avoids that codepath.
# ------------------------------------------
assignInNamespace("getSymbols.FRED", function(Symbols, env, return.class = "xts", ...) {
  
  args <- list(...)
  verbose      <- isTRUE(args$verbose)
  auto.assign  <- if (is.null(args$auto.assign)) TRUE else isTRUE(args$auto.assign)
  warnings     <- if (is.null(args$warnings))    TRUE else isTRUE(args$warnings)
  from         <- if (is.null(args$from))        ""   else args$from
  to           <- if (is.null(args$to))          ""   else args$to
  
  FRED.URL  <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id="
  returnSym <- Symbols
  noDataSym <- NULL
  fr <- NULL
  
  for (i in seq_along(Symbols)) {
    
    sym <- Symbols[[i]]
    
    if (verbose) cat("downloading ", sym, ".....\n\n")
    test <- try({
      
      tmp_file <- tempfile(fileext = ".csv")
      utils::download.file(
        paste0(FRED.URL, sym), tmp_file,
        method = "curl", mode = "wb", quiet = TRUE,
        extra = "--silent --retry 3"
      )
      
      raw <- utils::read.csv(tmp_file, na.strings = ".")
      unlink(tmp_file)
      
      fr  <- xts::xts(
        as.matrix(raw[, -1]),
        as.Date(raw[, 1], origin = "1970-01-01"),
        src = "FRED", updated = Sys.time()
      )
      
      dim(fr) <- c(NROW(fr), 1)
      colnames(fr) <- toupper(sym)
      fr <- fr[paste(from, to, sep = "/")]
      fr <- quantmod:::convert.time.series(fr = fr, return.class = return.class)
      
      if (auto.assign) {
        
        assign(toupper(gsub("\\^", "", sym)), fr, env)
        
      }
      
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
  
},

ns = "quantmod"

)

# source all modules
source("R/01-sp500.R")
source("R/02-goyal-welch.R")
source("R/03-kenneth-french.R")
source("R/04-pastor-stambaugh.R")
source("R/05-fred-api.R")
source("R/06-fred-md.R")
source("R/07-datastream-proxies.R")
source("R/08-uncertainty.R")
source("R/09-vix.R")
source("R/10-cochrane-piazzesi.R")
source("R/11-usrec.R")
source("R/12-technical-indicators.R")

# create output directories
dir.create("data/short", showWarnings = FALSE, recursive = TRUE)
dir.create("data/long", showWarnings = FALSE, recursive = TRUE)

# ==========================================
#              Download all data
# ------------------------------------------

cat("=== Starting data download ===\n\n")

# 1. S&P 500
sp500_data <- download_sp500()

sp500_daily   <- sp500_data$sp500_daily
sqret_daily   <- sp500_data$sqret_daily
sqret_monthly <- sp500_data$sqret_monthly
rv_stocks     <- sp500_data$rv_stocks
sp500_monthly <- sp500_data$sp500_monthly

# 2. Goyal-Welch predictors
gw_data   <- download_goyal_welch()
gw_pred   <- gw_data$gw_pred
erp_rfree <- gw_data$erp_rfree

# 3. Kenneth French factors (FF5 + Momentum + Short-Term Reversal)
kf_pred <- download_kenneth_french()

# 4. Pastor-Stambaugh liquidity
pastor_fct <- download_pastor_stambaugh()

# 5. Cochrane-Piazzesi factor
cp_fct <- download_cochrane_piazzesi()

# 6. FRED API macroeconomic variables
fred_api <- download_fred_api()

# 7. FRED-MD dataset (uses fredmd package)
fred_md <- download_fred_md(fredmd_path = "fredmd")

# 8. Datastream proxy variables
datastream <- download_datastream_proxies()

# 9. Uncertainty indices
uncertainty <- download_uncertainty()

# 10. VIX
vix <- download_vix()

# 11. US Recession indicator
usrec <- download_usrec()

cat("\n=== All downloads complete ===\n\n")

# ==========================================
#   Compute S&P 500 Technical Indicators
# ------------------------------------------

cat(">> Computing technical indicators...\n")

# prepare inputs (rownames = yyyymm date)
sp500_close <- sp500_monthly |>
  tibble::column_to_rownames(var = "date") |>
  select(close)

sp500_volume <- sp500_monthly |>
  tibble::column_to_rownames(var = "date") |>
  select(volume)

sp500_rv <- rv_stocks |>
  tibble::column_to_rownames(var = "yyyymm") |>
  select(rv)

# moving average signals
sp500_ti_ma <- get_ma(y_t = sp500_close, s_i = c(1, 2, 3), l_i = c(9, 12))

# momentum signals
sp500_ti_mom <- get_mom(y_t = sp500_close, m_i = c(9, 12))

# volume-based signals
sp500_ti_vol <- get_vol(
  y_t    = sp500_close,
  volume = sp500_volume,
  s_i    = c(1, 2, 3),
  l_i    = c(9, 12)
)

# volatility momentum signals
sp500_ti_voa <- get_voa(y_t = sp500_rv, m_i = c(1, 3, 6, 9, 12))

# merge technical indicators
sp500_technical_indicators <-
  purrr::reduce(
    list(sp500_ti_ma, sp500_ti_mom, sp500_ti_vol, sp500_ti_voa),
    left_join, by = "date"
  ) |>
  tidyr::drop_na() |>
  rename(yyyymm = date) |>
  mutate(
    year  = as.numeric(str_sub(as.character(yyyymm), 1, 4)),
    month = as.numeric(str_sub(as.character(yyyymm), -2))
  )

# ==========================================
#        Merge all predictor datasets
# ------------------------------------------

cat(">> Merging all predictors...\n")

proc_predictors <- list(
  gw_pred,                    # 1:  Goyal-Welch
  kf_pred,                    # 2:  Kenneth French (FF5 + Mom + STR)
  pastor_fct,                 # 3:  Pastor-Stambaugh
  cp_fct,                     # 4:  Cochrane-Piazzesi
  fred_api,                   # 5:  FRED macro
  fred_md,                    # 6:  FRED-MD
  datastream,                 # 7:  Datastream proxies
  uncertainty,                # 8:  Uncertainty indices
  vix,                        # 9:  VIX
  sp500_technical_indicators  # 10: Technical indicators
)

# find common start date (max of all minimums)
start_yyyymm <- 196001
cat("   Common start date (yyyymm):", start_yyyymm, "\n")

# merge all
predictor_data <- purrr::reduce(proc_predictors, left_join, by = c("yyyymm", "year", "month")) |>
  janitor::clean_names() |>
  rename_with(~ str_replace_all(., "_", "")) |>
  filter(yyyymm >= start_yyyymm) |>
  arrange(yyyymm)

cat("   Final dataset:", nrow(predictor_data), "rows x", ncol(predictor_data), "columns\n")

# ==========================================
#  Filter erp_rfree and usrec to same range
# ------------------------------------------

erp_rfree <- erp_rfree |> filter(yyyymm >= start_yyyymm)
usrec     <- usrec |> filter(yyyymm >= start_yyyymm)

# ==========================================
# Output: Short Sample (complete cases only)
# ------------------------------------------

cat(">> Saving Short Sample...\n")

predictor_data_short <- predictor_data |> 
  filter(yyyymm >= 199001)

wb <- createWorkbook()

addWorksheet(wb, "predictors")
writeData(wb, "predictors", predictor_data_short)

addWorksheet(wb, "square-returns")
writeData(wb, "square-returns", sqret_monthly)

addWorksheet(wb, "daily-returns")
writeData(wb, "daily-returns", sqret_daily)

addWorksheet(wb, "erp-rfree")
writeData(wb, "erp-rfree", erp_rfree)

addWorksheet(wb, "recession")
writeData(wb, "recession", usrec)

saveWorkbook(wb, "data/short/PredictorData.xlsx", overwrite = TRUE)
rm(wb)

# ==========================================
# Output: Long Sample (columns with no NAs only)
# ------------------------------------------

cat(">> Saving Long Sample...\n")

# Table D.1 footnote 'a': variables available only from 1990 onwards.
# Drop them for the long-sample (1960-) dataset (yields 157 predictors).
short_only_vars <- c(
  "msci", "rmw", "cma", "cp",
  "ps", "ted", "twexmmth", "cap",
  "sent", "conf", "diff", "pmbb",
  "andenox", "vxoclsx", "vix", "rabex",
  "uncbex", "epu", "finunc", "macrounc",
  "realunc", "usmpu"
)

predictor_data_long <- predictor_data |>
  filter(yyyymm >= 196001) |>
  dplyr::select(-dplyr::any_of(short_only_vars))

wb <- createWorkbook()

addWorksheet(wb, "predictors")
writeData(wb, "predictors", predictor_data_long)

addWorksheet(wb, "square-returns")
writeData(wb, "square-returns", sqret_monthly)

addWorksheet(wb, "daily-returns")
writeData(wb, "daily-returns", sqret_daily)

addWorksheet(wb, "erp-rfree")
writeData(wb, "erp-rfree", erp_rfree)

addWorksheet(wb, "recession")
writeData(wb, "recession", usrec)

saveWorkbook(wb, "data/long/PredictorData.xlsx", overwrite = TRUE)
rm(wb)

# ==========================================
#           Upload to Google Drive
# ------------------------------------------

cat(">> Uploading to Google Drive...\n")

# Authenticate. In CI a service-account JSON key is provided via
# GDRIVE_SA_KEY (file path). Locally we fall through to the cached
# OAuth token from googledrive's gargle cache.
sa_key <- Sys.getenv("GDRIVE_SA_KEY", unset = "")

if (nzchar(sa_key)) {

  cat("   Authenticating with Google service account...\n")
  googledrive::drive_auth(path = sa_key)

}

# Resolve the parent "monthdata" folder.
#   - GDRIVE_FOLDER_ID (CI): id of a folder shared with the service
#     account. Used directly because service accounts cannot search
#     the user's My Drive.
#   - Local: look up (or create) a folder literally named "monthdata"
#     in the authenticated user's Drive.
parent_id <- Sys.getenv("GDRIVE_FOLDER_ID", unset = "")

drive_find_or_mkdir <- function(name, path = NULL) {

  existing <- if (is.null(path)) {

    googledrive::drive_find(
      pattern = paste0("^", name, "$"),
      type = "folder", n_max = 1
    )

  } else {

    googledrive::drive_ls(path, type = "folder") |>
      dplyr::filter(name == !!name)

  }

  if (nrow(existing) > 0) {

    return(existing[1, ])

  }

  googledrive::drive_mkdir(name, path = path)

}

root_folder <- if (nzchar(parent_id)) {

  googledrive::as_id(parent_id)

} else {

  drive_find_or_mkdir("monthdata")

}

short_folder <- drive_find_or_mkdir("short", root_folder)
long_folder  <- drive_find_or_mkdir("long",  root_folder)

# helper: upload or update
drive_put_file <- function(local, folder, name) {
  
  existing <- googledrive::drive_ls(folder) |>
    dplyr::filter(name == !!name)
  
  if (nrow(existing) > 0) {
    
    googledrive::drive_update(existing[1, ], local)
    
  } else {
    
    googledrive::drive_upload(local, path = folder, name = name)
    
  }
  
}

drive_put_file("data/short/PredictorData.xlsx", short_folder, "PredictorData.xlsx")
drive_put_file("data/long/PredictorData.xlsx", long_folder, "PredictorData.xlsx")

cat("   Uploaded to Google Drive:", "monthdata/short/ & monthdata/long/\n")
cat("\n=== Done! ===\n")
