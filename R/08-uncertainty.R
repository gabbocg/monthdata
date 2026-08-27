# ---------------------------------------------------------------
# Uncertainty Indices
# Sources:
#   - rabex/uncbex: Bekaert, Engstrom & Xu (2022), nancyxu.net
#   - gprh/gprht/gprha: Caldara & Iacoviello (2022),
#                        matteoiacoviello.com
#   - epu:   Baker, Bloom & Davis via FRED (USEPUINDXM)
#   - finunc/macrounc/realunc: Jurado, Ludvigson & Ng (2015),
#                              sydneyludvigson.com
#   - usmpu: Husted, Rogers & Sun (2020),
#            policyuncertainty.com
# ---------------------------------------------------------------

download_uncertainty <- function(from = "1959-01-01", to = Sys.Date()) {
  
  cat(">> Downloading uncertainty indices...\n")
  
  # --- EPU from FRED ---
  cat("   EPU from FRED...\n")
  epu <- tq_get_retry(
    "USEPUINDXM",
    get   = "economic.data",
    from  = from,
    to    = as.character(to),
    label = "EPU (USEPUINDXM, FRED)"
  ) |>
    dplyr::mutate(
      month  = lubridate::month(date),
      year   = lubridate::year(date),
      yyyymm = as.numeric(
        paste0(year, sprintf("%02d", month))
      ),
      epu = price
    ) |>
    dplyr::select(yyyymm, year, month, epu)
  
  # --- RABEX + UNCBEX: Bekaert-Engstrom-Xu ---
  cat("   RABEX/UNCBEX from nancyxu.net...\n")
  bex <- tryCatch({
    rabex_url <- paste0(
      "https://www.nancyxu.net/_files/ugd/",
      "efd0b7_cc1fb93a486244bc820dd1bdd8f7a7d5",
      ".xlsx?dn=BEX_Indices_20260403.xlsx"
    )
    tmp <- tempfile(fileext = ".xlsx")
    download.file(
      rabex_url, tmp,
      mode = "wb", method = "curl", quiet = TRUE
    )
    df <- readxl::read_xlsx(
      tmp, sheet = "DATA_PLOT_monthly"
    )
    unlink(tmp)
    
    df |>
      dplyr::select(
        yyyymm, ra_bex_PAPER, unc_bex_PAPER
      ) |>
      dplyr::rename(
        rabex  = ra_bex_PAPER,
        uncbex = unc_bex_PAPER
      ) |>
      tidyr::drop_na() |>
      dplyr::mutate(
        yyyymm = as.numeric(yyyymm),
        year   = as.numeric(
          stringr::str_sub(yyyymm, 1, 4)
        ),
        month  = as.numeric(
          stringr::str_sub(yyyymm, -2)
        )
      ) |>
      dplyr::select(
        yyyymm, year, month, rabex, uncbex
      )
  }, error = function(e) {
    cat(
      "   WARNING: BEX download failed.",
      "Check nancyxu.net URL.\n"
    )
    tibble::tibble(
      yyyymm = integer(), year = integer(),
      month = integer(), rabex = double(),
      uncbex = double()
    )
  })
  
  # --- GPR: Caldara-Iacoviello Geopolitical Risk ---
  cat("   GPR from matteoiacoviello.com...\n")
  gpr <- tryCatch({
    
    gpr_url <- paste0(
      "https://www.matteoiacoviello.com",
      "/gpr_files/data_gpr_export.xls"
    )
    tmp <- tempfile(fileext = ".xls")
    download.file(
      gpr_url, tmp,
      mode = "wb", method = "curl", quiet = TRUE
    )
    df <- readxl::read_xls(tmp)
    unlink(tmp)
    
    df |>
      dplyr::select(month, GPRH, GPRHT, GPRHA) |>
      dplyr::rename(
        date  = month,
        gprh  = GPRH,
        gprht = GPRHT,
        gprha = GPRHA
      ) |>
      tidyr::drop_na(date) |>
      dplyr::mutate(
        month  = lubridate::month(date),
        year   = lubridate::year(date),
        yyyymm = as.numeric(
          paste0(year, sprintf("%02d", month))
        )
      ) |>
      dplyr::select(
        yyyymm, year, month,
        gprh, gprht, gprha
      )
    
  }, error = function(e) {
    
    cat("   WARNING: GPR download failed.\n")
    tibble::tibble(
      yyyymm = integer(), year = integer(),
      month = integer(), gprh = double(),
      gprht = double(), gprha = double()
    )
    
  })
  
  # --- JLN: Jurado-Ludvigson-Ng Uncertainty ---
  cat("   JLN uncertainty from sydneyludvigson.com...\n")
  jln <- tryCatch({
    
    jln_url <- paste0(
      "https://www.sydneyludvigson.com/s/",
      "MacroFinanceUncertainty_202602Update",
      "-3klg.zip"
    )
    tmp_zip <- tempfile(fileext = ".zip")
    # Squarespace blocks R's download.file; use system curl
    system2(
      "curl", c("-sL", "-o", tmp_zip, jln_url)
    )
    
    if (file.size(tmp_zip) == 0) {
      
      stop("JLN download returned empty file")
      
    }
    
    tmp_dir <- tempdir()
    unzip(tmp_zip, exdir = tmp_dir)
    unlink(tmp_zip)
    
    read_jln <- function(file, col_name) {
      
      df <- readxl::read_xlsx(
        file.path(tmp_dir, file)
      )
      df |>
        dplyr::select(Date, `h=1`) |>
        dplyr::rename(
          date = Date, !!col_name := `h=1`
        ) |>
        tidyr::drop_na() |>
        dplyr::mutate(
          month  = lubridate::month(date),
          year   = lubridate::year(date),
          yyyymm = as.numeric(
            paste0(year, sprintf("%02d", month))
          )
        ) |>
        dplyr::select(
          yyyymm, year, month,
          dplyr::all_of(col_name)
        )
      
    }
    
    fin <- read_jln("FinancialUncertaintyToCirculate.xlsx", "finunc")
    mac <- read_jln("MacroUncertaintyToCirculate.xlsx", "macrounc")
    rea <- read_jln("RealUncertaintyToCirculate.xlsx", "realunc")
    
    fin |>
      dplyr::full_join(
        mac, by = c("yyyymm", "year", "month")
      ) |>
      dplyr::full_join(
        rea, by = c("yyyymm", "year", "month")
      )
    
  }, error = function(e) {
    
    cat("   WARNING: JLN download failed.\n")
    tibble::tibble(
      yyyymm = integer(), year = integer(),
      month = integer(), finunc = double(),
      macrounc = double(), realunc = double()
    )
    
  })
  
  # --- USMPU: Monetary Policy Uncertainty ---
  cat("   USMPU from policyuncertainty.com...\n")
  usmpu <- tryCatch({
    
    url <- paste0(
      "https://www.policyuncertainty.com",
      "/media/US_MPU_Monthly.xlsx"
    )
    
    tmp <- tempfile(fileext = ".xlsx")
    download.file(url, tmp, mode = "wb", method = "curl", quiet = TRUE)
    
    df <- readxl::read_xlsx(tmp) |>
      janitor::clean_names()
    
    unlink(tmp)
    
    mpu_col <- grep(
      "mpu|index", colnames(df),
      ignore.case = TRUE, value = TRUE
    )[1]
    if (is.na(mpu_col)) mpu_col <- colnames(df)[3]
    
    df |>
      dplyr::filter(!is.na(month)) |>
      dplyr::mutate(
        year   = as.numeric(
          stringr::str_extract(year, "\\d{4}")
        ),
        month  = as.numeric(month),
        yyyymm = as.numeric(
          paste0(year, sprintf("%02d", month))
        ),
        usmpu  = as.numeric(.data[[mpu_col]])
      ) |>
      dplyr::select(yyyymm, year, month, usmpu)
    
  }, error = function(e) {
    
    cat("   WARNING: USMPU download failed.\n")
    tibble::tibble(
      yyyymm = integer(), year = integer(),
      month = integer(), usmpu = double()
    )
    
  })
  
  # merge all uncertainty indices
  uncertainty <- epu |>
    dplyr::full_join(
      bex, by = c("yyyymm", "year", "month")
    ) |>
    dplyr::full_join(
      gpr, by = c("yyyymm", "year", "month")
    ) |>
    dplyr::full_join(
      jln, by = c("yyyymm", "year", "month")
    ) |>
    dplyr::full_join(
      usmpu, by = c("yyyymm", "year", "month")
    ) |>
    dplyr::arrange(yyyymm)
  
  uncertainty
  
}
