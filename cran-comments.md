This is a minor release including new functionality 

New functions are
- nb_pi()
- neg_bin_pi()
- rnbinom()


## R CMD check results (local on my 2021 MacBook, M1pro)
0 errors | 0 warnings | 0 note

## Checks with devtools::check()
0 errors ✔ | 0 warnings ✔ | 0 notes ✔ 


## Checks with devtools::check_rhub()

 Checks on the Windows Server 2022, R-devel, 64 bit gave the following notes: 
- Found the following files/directories: 'lastMiKTeXException'
- Found the following files/directories: ''NULL''
- Skipping checking math rendering: package 'V8' unavailable

 Checks on Ubuntu Linux 20.04.1 LTS, R-release, GCC
- checking HTML version of manual ... :
    - Skipping checking HTML validation: no command 'tidy' found
    - Skipping checking math rendering: package 'V8' unavailable

Checks with Fedora Linux, R-devel, clang, gfortran
- checking HTML version of manual ...:
    - Skipping checking HTML validation: no command 'tidy' found
    - Skipping checking math rendering: package 'V8' unavailable


This seems to be some issues with the servers itself and not with my package....


## Checks with devtools::check_win_devel()

- checking CRAN incoming feasibility ... [11s] NOTE:
    - Maintainer: 'Max Menssen <menssen@cell.uni-hannover.de>'

"This is just a Note that reminds CRAN maintainers to check that the 
submission comes actually from his maintainer and not anybody else." 
see post of Uwe Ligges: https://mailman.stat.ethz.ch/pipermail/r-devel/2014-March/068497.html


