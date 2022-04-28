## R CMD check results (local on my MacBook)

0 errors | 0 warnings | 0 note


## Test environments

Below, you can find a list of the environments/functions I used for testing my
package.

If an error occured, I reported it. Anyhow, as far as I understand, the reported 
error gives a hint on problems of the particular test environment itself, rather 
than on problems with my package.

### Test with devtools::check_win_*()

* check_win_devel()

* check_win_release()

* check_win_oldrelease()
  1 Error:
        running tests for arch 'x64' ... [1s] ERROR
        Running 'testthat.R' [0s]
        Running the tests in 'tests/testthat.R' failed.
        Complete output:
          > library(testthat)
          Error in library(testthat) : there is no package called 'testthat'
          Execution halted
          
        -> It seems that testthat is not installed on the test-environment, which 
           if I got it right is not a problem of my package itself.

### Tests with rhub::check()

* Debian Linux, R-devel, clang, ISO-8859-15 locale

* Fedora Linux, R-devel, clang, gfortran

* macOS 10.13.6 High Sierra, R-release, CRAN's setup

* Ubuntu Linux 20.04.1 LTS, R-devel, GCC



