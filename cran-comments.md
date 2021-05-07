## Test environments
* local R installation, R 4.0.5
* ubuntu 16.04 (on travis-ci), R 4.0.5
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 note

The examples for bb_dat1 and lmer_pi might elapse the time of 5s. I already 
reduced the number of bootstreps to 100, in order to increase computing time.
Anyhow, with less than 100 bootstraps the functionallity of the lmer_pi() function
can not be explained properly.

* This is a new release.
