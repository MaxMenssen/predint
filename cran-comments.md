This is a minor release including three new functions

* lmer_pi_unstruc()
* lmer_pi_futvec()
* lmer_pi_futmat()


## R CMD check results (local on my MacBook)

0 errors | 0 warnings | 0 note


## Checks with devtools::check_rhub()

All checks seem to be ok. Only the Checks on the Windows Server 2022, R-devel, 64 bit
gave the following note: Found the following files/directories: 'lastMiKTeXException'



## Checks with devtools::check_win_devel()

The Check gave following Note:

Found the following (possibly) invalid URLs:
  URL: https://onlinelibrary.wiley.com/doi/10.1002/sim.8124
    From: README.md
    Status: 503
    Message: Service Unavailable
  URL: https://onlinelibrary.wiley.com/doi/10.1111/stan.12260?af=R
    From: README.md
    Status: 503
    Message: Service Unavailable
    
Due to Hadley Wickham these Notes can be ignored (see link below). Hope that´s right.
https://twitter.com/hadleywickham/status/1358170607314235392
