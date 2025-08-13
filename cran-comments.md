## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

* Note:
  - `checking for future file timestamps ... NOTE`
    The note is due to CRAN check environment time verification. 
    This does not affect package functionality and is unrelated to the package code.

* Maintainer NOTE (spelling):
  - "EDA" refers to exploratory data analysis.
  - "ZINB" refers to zero-inflated negative binomial models.
    Both are standard statistical abbreviations used intentionally.

Additional testing:
- Local: Windows 10 (R 4.5.1) — all checks passed.
- rhub: macOS (arm64), Windows (latest), Ubuntu 22.04 (release) — all checks passed.
- win-builder (R-release, R-devel) — only the above Maintainer NOTE observed.
