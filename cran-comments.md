This is a major release including new functionality based on the S3 system

New functions are
- as.data.frame.predint
- bb_pi
- bisection
- boot_predint
- coverage_prob
- normal_pi
- plot.predint
- print.predint
- qb_pi
- qp_pi
- summary.predint


## R CMD check results (local on my 2021 MacBook, M1pro)

0 errors | 0 warnings | 1 note

checking for future file timestamps ... NOTE
  unable to verify current time
  
This note seems to be a documented problem with CMD check and should not cause any problem, see
https://stackoverflow.com/questions/63613301/r-cmd-check-note-unable-to-verify-current-time



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




