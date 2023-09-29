This is a minor release including new functionality 

New functions are
- nb_pi()
- neg_bin_pi()
- rnbinom()


## R CMD check results (local on my 2021 MacBook, M1pro)

0 errors | 0 warnings | 0 note




## Checks with devtools::check_rhub()

All checks seem to be ok. Only the Checks on the Windows Server 2022, R-devel, 64 bit
gave the following notes: 
- Found the following files/directories: 'lastMiKTeXException'
- Skipping checking math rendering: package 'V8' unavailable

This seems to be some problems with the server itself and not with my code....




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
    
I checked the README on Github and all links work fine.

Due to Hadley Wickham these Notes can be ignored (see link below). Hope that´s right.
https://twitter.com/hadleywickham/status/1358170607314235392




