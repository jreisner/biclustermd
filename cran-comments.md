## Test Environments
* local OS X, R 3.6.0
* win-builder (devel, release, and old release)
* Rhub
    * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
    * Ubuntu Linux 16.04 LTS, R-release, GCC
    * Fedora Linux, R-devel, clang, gfortran

## R CMD Check Results
0 errors | 0 warnings | 0 notes


## Other notes
The changes since v0.2.1:
1. two tests fixed to work with dplyr v1.0.0 release.
2. discontinued the use of the deprecated dplyr::group_indices() and replaced with a base R equivalent.
