## Test Environments
* local OS X, R 3.6.0
* win-builder (devel and release)

## R CMD Check Results
0 errors | 0 warnings | 0 notes

## Other notes
This is the fourth submission of this package to CRAN.
* Changes since third submission:
  * Removed `gg_ri()` as it is unexported and deprecated.
  * Removed `gg_sse()` as it is unexported and deprecated.
  * Removed `ggplot` method as it is unexported and deprecated.
  * Removed `.packages = 'biclustermd'` from `rep_biclustermd()`, which I believe threw the cited error in `tune_biclustermd()`.
  * Removed `.packages = 'biclustermd'` from `tune_biclustermd()`, which I believe threw the cited error.
