## Test environments
* local R installation, R 4.0.0
* ubuntu 16.04 (on travis-ci), R 4.0.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 note

check_rhub() and check_win_devel() gave the note that this is a new release, but 
nothing more.


The following comments were given by Julia Haider via e-mail:

1) Please do not start the description with "This package", package name, title or similar.
-> I fixed this

2) If there are references describing the methods in your package, please add these in the   description field of your DESCRIPTION file....
-> The paper regarding the methods used in this package is still in review. Hence,
   there is no reference other than the link on the cran homepage that will be 
   active after the upload. 

* This is a new release.
