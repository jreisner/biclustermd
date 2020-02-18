## Test Environments
* local OS X, R 3.6.0
* win-builder (devel, release, and old release)
* Rhub
    * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
    * Ubuntu Linux 16.04 LTS, R-release, GCC
    * Fedora Linux, R-devel, clang, gfortran

## R CMD Check Results
0 errors | 0 warnings | 0 notes

Except for Windows Server 2008 R2 SP1, R-devel, 32/64 bit: 0 errors | 1 warning | 0 notes
+ `Requires orphaned packages: 'biclust'`
    * looks like biclust is no longer orphaned on CRAN


## Other notes
The changes since v0.2.0:
1. two tests fixed to work with ggplot2 v3.3.0 release.
2. discontinued the use of the orphaned clues package. Now uses the clusteval and phyclust packages.
