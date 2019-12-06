## Initial release to CRAN and first official release

## Changes since v0.1.0
+ `autoplot.biclustermd()` now has an `axis.text` argument which allows users to 
    specify along which axes ticks and text should be drawn. See `?autoplot.biclustermd`
    for more information. (Issue #1)
+ `autoplot.biclustermd_sim()` now puts an asterisk at the end of the similariity 
    measure used in `biclustermd()` when plotting. (#3)
+ `autoplot.biclustermd_sim()` is equipped with a `similarity` argument which
    specifies the similarity measure to plot. (#2)
+ All uses of `class()` are replaced with `inherits()` in preparation for R 4.0.0
+ `gather.biclustermd()` now returns the correct bicluster numbering.
