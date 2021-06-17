## Test Environments
* local OS X, R 3.6.0
* win-builder (devel, release, and old release)
* Rhub
    * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
    * Ubuntu Linux 20.04.1 LTS, R-release, GCC
    * Fedora Linux, R-devel, clang, gfortran

## R CMD Check Results
0 errors | 0 warnings | 0 notes


## Other notes
The changes since v0.2.2:
1. clusteval is looking like it'll be archived, so I wrote a jaccard_similarity() function to replace clusteval::cluster_similarity().
