## R CMD check results

0 errors | 0 warnings | 1 note

* This is a resubmission.

- Fixed the DESCRIPTION field to use only undirected quotation marks and 
  removed quotation marks around function names (e.g., compare_monthly_cases()).
- Fixed the DESCRIPTION field to include references describing the methods 
  in the required format.
- Fixed all console output by replacing direct use of print()/cat() with 
  message(), warning(), or conditional verbose output where appropriate, 
  and ensured informational results are returned as objects.
  * Fixed in: R/check_rate_diff_arima_ready.R, R/plot_weekly_cases.R
- Fixed LICENSE specification according to CRAN feedback.
- Bumped version 1.0.1
  


* Note:
  - `checking for future file timestamps ... NOTE`
    The note is due to CRAN check environment time verification. 
    This does not affect package functionality and is unrelated to the package code.

* Maintainer NOTE (spelling):
  - "EDA" refers to exploratory data analysis.
  - "ZINB" refers to zero-inflated negative binomial models.
    Both are standard statistical abbreviations used intentionally.
  - "Achim" and others are author names.

Additional testing:
- Local: Windows 10 (R 4.5.1) — all checks passed.
- rhub: macOS (arm64), Windows (latest), Ubuntu 22.04 (release) — all checks passed.
- win-builder (R-release, R-devel) — only the above Maintainer NOTE observed.
