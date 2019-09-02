## Test Environments
* local OS X, R 3.6.0
* win-builder (devel and release)

## R CMD Check Results
0 errors | 0 warnings | 0 notes

## Other notes
This is the third submission of this package to CRAN.
* Changes since second submission:
  * Removed `old_bicluster()` as it just redirected to `biclustermd()`
  * Uncommented an example in `as.Biclust()` and wrapped `\dontrun{}` around it, since that example fails, and the example is there specifically to show that behavior.
  * Replaced "NP-hard" with its definition in the description.
  * Added the reference for the algorithm to the description.
