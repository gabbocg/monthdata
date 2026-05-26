# Monthly Predictors for Realized-Volatility Forecasting

[![R](https://img.shields.io/badge/R-%3E%3D4.0-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)
[![FRED-MD](https://img.shields.io/badge/Data-FRED--MD-1f6feb)](https://www.stlouisfed.org/research/economists/mccracken/fred-databases)

R pipeline that downloads, transforms, and merges the 179 monthly predictors from Table D.1 of Díaz, Hansen & Cabrera (2024, IRFA).

## Overview

The paper "Machine-Learning Stock Market Volatility" uses two panels of monthly predictors to forecast the realized volatility of the S&P 500: a **short sample** (1990–) with 179 variables and a **long sample** (1960–) with 157 variables (the 22 series flagged in Table D.1 with footnote `a` are only available from 1990 onwards and are dropped from the long panel).

This repository is **data collection only** — it pulls each predictor from its original public source, applies the transformation conventions used in the paper, and writes two Excel workbooks ready for downstream modelling. No estimation code lives here.

The pipeline covers ten predictor groups:

- Equity / risk factors (Goyal-Welch, Fama-French 5 + Momentum + STR, MSCI World, Pastor-Stambaugh liquidity)
- Interest rates, spreads, and bond-market factors (FRED-MD + Cochrane-Piazzesi)
- Foreign-exchange and trade-weighted indices
- Macroeconomic real-activity series (FRED + FRED-MD)
- Inflation, prices, and monetary aggregates
- Survey / sentiment indicators
- Labour-market and housing series
- Uncertainty indices (VIX, VXO, BEX, GPR, EPU, JLN, USMPU)
- Financial uncertainty / risk aversion
- Technical indicators on the S&P 500 (MA, MOM, VOL, RV signals)

## Modules

Each script in `R/` exports a single `download_*()` function that returns a tibble keyed by `yyyymm, year, month`.

| Module | Source | Variables |
|--------|--------|-----------|
| `R/01-sp500.R` | Yahoo Finance | S&P 500 prices, realized volatility, squared returns |
| `R/02-goyal-welch.R` | Amit Goyal | `dp, ep, tb, ltr, ts, def, rtb, rbr, infm` (+ `erp`, `rfree` in a separate sheet) |
| `R/03-kenneth-french.R` | Ken French Data Library | `mkt, smb, hml, mom, rmw, cma, str` |
| `R/04-pastor-stambaugh.R` | Chicago Booth | `ps` |
| `R/05-fred-api.R` | FRED | `ipm, ipa, m1m, m1a, cap, empl, sent, hs` |
| `R/06-fred-md.R` | FRED-MD + [`fredmd`](https://github.com/GabboCg/fredmd) | 113 transformed macro series (whitelisted to Table D.1; `MZMSL` pulled directly from FRED) |
| `R/07-datastream-proxies.R` | FRED / Yahoo | `ordm, orda, infa, msci, crb, pmi, pmbb, conf, ted, diff` |
| `R/08-uncertainty.R` | FRED / nancyxu.net / matteoiacoviello.com / sydneyludvigson.com / policyuncertainty.com | `epu, rabex, uncbex, gprh, gprht, gprha, finunc, macrounc, realunc, usmpu` |
| `R/09-vix.R` | FRED (VIXCLS) | `vix` |
| `R/10-cochrane-piazzesi.R` | Computed from FRED yields | `cp` |
| `R/11-usrec.R` | FRED (USREC) | `usrec` (auxiliary sheet) |
| `R/12-technical-indicators.R` | Computed from S&P 500 | 6 MA, 2 MOM, 6 VOL, 5 RV binary signals |

## Output

Running the pipeline writes two workbooks:

```
data/
  short/PredictorData.xlsx   # 1990-present, 179 predictors
  long/PredictorData.xlsx    # 1960-present, 157 predictors
```

Each workbook contains five sheets:

| Sheet | Contents |
|-------|----------|
| `predictors` | Table D.1 predictors, keyed by `yyyymm` |
| `square-returns` | Monthly sum of squared daily S&P 500 log-returns |
| `daily-returns` | Daily squared log-returns |
| `erp-rfree` | Equity risk premium and risk-free rate |
| `recession` | NBER recession dummy (`USREC`) |

## Download

The latest refresh of both workbooks is published to Google Drive and updated automatically each day by the [`refresh-data`](.github/workflows/refresh-data.yml) workflow.

| Sample | Period | Predictors | Link |
|--------|--------|------------|------|
| Short  | 1990–present | 179 | [PredictorData.xlsx](https://docs.google.com/spreadsheets/d/1t9lgsI_JUWdkEaZsyfm_sd85m7NHStlH/edit?usp=sharing) |
| Long   | 1960–present | 157 | [PredictorData.xlsx](https://docs.google.com/spreadsheets/d/1MwsjDhoAaUnnIdwKRcmBsPSOgvxFlIzk/edit?usp=sharing) |

For a direct `.xlsx` download instead of the in-browser viewer: 

- [Short Data](https://docs.google.com/spreadsheets/d/1t9lgsI_JUWdkEaZsyfm_sd85m7NHStlH/export?format=xlsx) 
- [Long Data](https://docs.google.com/spreadsheets/d/1MwsjDhoAaUnnIdwKRcmBsPSOgvxFlIzk/export?format=xlsx)

## Usage

From the project root:

```bash
Rscript download_data.R
```

On first run, the script clones the [`fredmd`](https://github.com/GabboCg/fredmd) repository alongside this project for FRED-MD processing (compiles a C++ factor-estimation routine via Rcpp / RcppArmadillo). Output directories are created automatically.

Required R packages:

```r
install.packages(c(
  "tidyquant", "dplyr", "tidyr", "purrr", "lubridate",
  "readxl", "stringr", "janitor", "magrittr", "openxlsx",
  "readr", "TTR", "PerformanceAnalytics", "Rcpp",
  "RcppArmadillo", "googledrive"
))
```

## Notes

- **Long-sample filter.** The 22 variables flagged in Table D.1 with footnote `a` are dropped from the long-sample Excel: `MSCI, RMW, CMA, CP, PS, TED, TWEXMMTH, CAP, SENT, CONF, DIFF, PMBB, ANDENOX, VXOCLSX, VIX, RABEX, UNCBEX, EPU, FINUNC, MACROUNC, REALUNC, USMPU`.

- **FRED-MD naming.** Legacy FRED-MD codes are renamed to the Table D.1 abbreviations: `WPSFD49207 → PPIFGS`, `WPSFD49502 → PPIFCG`, `WPSID61 → PPIITM`, `WPSID62 → PPICRM`, `TWEXAFEGSMTHx → TWEXMMTH`, `VIXCLSx → VXOCLSX`. `MZMSL` was removed from recent FRED-MD vintages, so it is pulled directly from FRED and transformed with FRED-MD tcode 6 (second log difference).

- **Datastream proxies.** Several Table D.1 variables originally sourced from Datastream (Refinitiv) are replaced with the closest free public series — `MSCI` (URTH ETF), `ORDM/ORDA` (`AMTMNO`), `INFA` (`CPIAUCSL`), `CRB` (`PALLFNFINDEXM`), `PMI` (`BSCICP02USM460S`), `PMBB` (`GACDFSA066MSFRBPHI`), `CONF` (`CSCICP03USM665S`), `TED` (`TB3MS`), `DIFF` (`USPHCI`). Users with Datastream access can swap these for the originals.

- **FRED downloader override.** `download_data.R` patches `quantmod::getSymbols.FRED` to use the system `curl` binary instead of `curl::curl()`; this works around intermittent `HTTP/2 stream not closed cleanly` errors from FRED on macOS/libcurl.

## References

- Díaz, J.D., Hansen, E., & Cabrera, G. (2024). Machine-learning stock market volatility: Predictability, drivers, and economic value. *International Review of Financial Analysis*, 94, 103286.

- McCracken, M.W., & Ng, S. (2016). FRED-MD: A monthly database for macroeconomic research. *Journal of Business & Economic Statistics*, 34(4), 574–589.

- Goyal, A., & Welch, I. (2008). A comprehensive look at the empirical performance of equity premium prediction. *Review of Financial Studies*, 21(4), 1455–1508.

- Neely, C.J., Rapach, D.E., Tu, J., & Zhou, G. (2014). Forecasting the equity risk premium: The role of technical indicators. *Management Science*, 60(7), 1772–1791.
