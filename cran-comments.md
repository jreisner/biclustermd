## Test Environments
* local OS X, R 3.6.0
* win-builder (devel and release)

## R CMD Check Results
0 errors | 0 warnings | 0 notes

## Other notes
This is the second submission of this package to CRAN.
* Changes since first submission:
  * Removed examples for `cluster_iteration_sum_sse()`
  * Removed examples for `results_heatmap()`
  * Replaced `\dontrun{}` in `tune_biclustermd()` examples with `\donttest{}`
* Reviewer cited `tune_biclustermd()` and claimed an example uses `ggplot.biclustermd()`. The example uses the standard `ggplot2::ggplot()` function.
