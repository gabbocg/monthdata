# ---------------------------------------------------------------
# FRED-MD Macroeconomic Dataset
# Source: McCracken & Ng, St. Louis Fed
# Processing: github.com/gabbocg/fredmd
#
# Full pipeline (matching fredmd/load.R):
#   1. Download latest FRED-MD CSV
#   2. Extract transformation codes (row 1)
#   3. Apply McCracken-Ng transformations (prepare_missing)
#   4. Remove outliers (10x IQR from median)
#   5. EM algorithm: impute missing + extract factors (C++)
#   6. Return balanced dataset (no NAs)
# ---------------------------------------------------------------

download_fred_md <- function(fredmd_path = "fredmd", demean = 2, jj = 2, kmax = 8) {
  
  cat(">> Downloading and processing FRED-MD dataset...\n")
  
  # clone fredmd repo if not present
  if (!dir.exists(fredmd_path)) {
    
    cat("   Cloning fredmd repository...\n")
    system2(
      "git",
      c("clone",
        "https://github.com/gabbocg/fredmd.git",
        fredmd_path),
      stdout = FALSE, stderr = FALSE
    )
    
  }
  
  # download latest FRED-MD CSV
  fred_md_url <- paste0(
    "https://www.stlouisfed.org",
    "/-/media/project/frbstl/stlouisfed",
    "/research/fred-md/monthly/current.csv"
  )
  
  csv_path <- file.path(fredmd_path, "Data", "current.csv")
  
  dir.create(file.path(fredmd_path, "Data"), showWarnings = FALSE, recursive = TRUE)
  
  download.file(fred_md_url, csv_path, mode = "wb", method = "curl", quiet = TRUE)
  
  # source R functions
  source(file.path(fredmd_path, "R", "prepare-missing.R"), local = TRUE)
  source(file.path(fredmd_path, "R", "remove-outliers.R"), local = TRUE)
  
  # compile and source C++ factor estimation
  cat("   Compiling C++ source...\n")
  Rcpp::sourceCpp(file.path(fredmd_path, "src", "fred_factors.cpp"))
  
  # step 1: read raw data
  raw_fred_md <- readr::read_csv(
    csv_path, show_col_types = FALSE
  )
  
  # step 2: extract transformation codes and data
  col_names <- colnames(raw_fred_md)[-1]
  tcode <- raw_fred_md[1, -1]
  raw_fred_md <- raw_fred_md |> dplyr::slice(-1)
  
  # step 3: apply McCracken-Ng transformations
  cat("   Applying transformations...\n")
  transformed_fred_md <- prepare_missing(raw_fred_md, tcode, vardate = "sasdate")
  
  # step 4: convert date format and drop first 2 rows
  transformed_fred_md <- transformed_fred_md |>
    dplyr::mutate(
      yyyymm = as.Date(yyyymm, format = "%m/%d/%Y")
    ) |>
    dplyr::slice(-1:-2)
  
  # step 5: remove outliers (10x IQR from median)
  cat("   Removing outliers...\n")
  transformed_fred_md <- remove_outliers(rawdata = transformed_fred_md)
  
  # step 6: EM algorithm — impute missing + factors
  cat("   Running EM factor estimation...\n")
  current_date <- transformed_fred_md$yyyymm
  
  x_mat <- as.matrix(transformed_fred_md[, -1])
  result <- factors_em_cpp(x_mat, kmax, jj, demean)
  
  # result[[5]] is the balanced (imputed) data matrix
  colnames(result[[5]]) <- colnames(x_mat)
  
  fred_md_balanced <- result[[5]] |>
    tibble::as_tibble() |>
    dplyr::mutate(date = current_date) |>
    dplyr::relocate(date, .before = 1) |>
    janitor::clean_names() |>
    tidyr::drop_na()
  
  # step 7: format to yyyymm
  fred_md_balanced <- fred_md_balanced |>
    dplyr::mutate(
      year   = lubridate::year(date),
      month  = lubridate::month(date),
      yyyymm = as.numeric(
        paste0(year, sprintf("%02d", month))
      )
    ) |>
    dplyr::select(-date) |>
    dplyr::relocate(yyyymm, year, month)
  
  # rename FRED-MD legacy codes to Table D.1 abbreviations
  fred_md_balanced <- fred_md_balanced |>
    dplyr::rename(
      ppifgs   = wpsfd49207,
      ppifcg   = wpsfd49502,
      ppiitm   = wpsid61,
      ppicrm   = wpsid62,
      twexmmth = twexafegsmt_hx,
      vxoclsx  = vixcl_sx
    )
  
  # remove variables duplicated in other scripts and series not in Table D.1
  vars_to_remove <- c(
    # duplicated in other scripts
    "indpro", "m1sl", "payems", "houst",
    "cpiaucsl", "tb3ms", "gs10", "aaa", "baa",
    "umcsen_tx",
    # not in Table D.1
    "acogno", "s_p_500", "s_p_div_yield", "s_p_pe_ratio"
  )
  
  fred_md_balanced <- fred_md_balanced |>
    dplyr::select(-dplyr::any_of(vars_to_remove))
  
  # MZMSL was removed from recent FRED-MD vintages but is in Table D.1.
  # Pull directly from FRED and apply tcode 6 (2nd log diff) to match the
  # FRED-MD transformation convention.
  cat("   Adding MZMSL from FRED (tcode 6)...\n")
  mzmsl_raw <- tidyquant::tq_get(
    "MZMSL",
    get  = "economic.data",
    from = "1959-01-01",
    to   = as.character(Sys.Date())
  ) |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      log_x  = log(price),
      mzmsl  = (log_x - dplyr::lag(log_x, 1)) -
        (dplyr::lag(log_x, 1) - dplyr::lag(log_x, 2)),
      year   = lubridate::year(date),
      month  = lubridate::month(date),
      yyyymm = as.numeric(
        paste0(year, sprintf("%02d", month))
      )
    ) |>
    dplyr::select(yyyymm, mzmsl)
  
  fred_md_balanced <- fred_md_balanced |>
    dplyr::left_join(mzmsl_raw, by = "yyyymm")
  
  cat(
    "   FRED-MD:", ncol(fred_md_balanced) - 3,
    "variables,", nrow(fred_md_balanced),
    "observations\n"
  )
  
  fred_md_balanced
  
}
