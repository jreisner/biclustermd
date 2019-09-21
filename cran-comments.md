## Test Environments
* local OS X, R 3.6.0
* win-builder (devel and release)

## R CMD Check Results
0 errors | 0 warnings | 0 notes

## Other notes
This is the fifth submission of this package to CRAN.
* Changes since fourth submission:
  * In `biclustermd()`, coerce `data` to matrix if `data` is a data.frame.
  * `rep_biclustermd()`: Evaluate function call if necessary. Added .export = 'biclustermd' to foreach call. Fixes a potential error, `Error in { : task 1 failed - "object 'biclustermd' not found"`
  * `tune_biclustermd()`: Added .export = 'biclustermd' to foreach call. Fixes the cited error, `Error in { : task 1 failed - "object 'rep_biclustermd' not found"`.
