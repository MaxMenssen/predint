
#-------------------------------------------------------------------------------
# bb_dat1

#' Beta-binomial data (example 1)
#'
#' This data set contains sampled beta-binomial data from 10 clusters
#' each of size 50. The data set was sampled with \code{rbbinom(n=10, size=50, prob=0.1, rho=0.06)}.
#'
#' @format A \code{data.frame} with 10 rows and 2 columns:
#' \describe{
#'   \item{succ}{number of successes}
#'   \item{fail}{number of failures}
#' }
#'
"bb_dat1"

#-------------------------------------------------------------------------------
# bb_dat2

#' Beta-binomial data (example 2)
#'
#' This data set contains sampled beta-binomial data from 3 clusters each of
#' different size. The data set was sampled with \code{rbbinom(n=3, size=c(40, 50, 60), prob=0.1, rho=0.06)}.
#'
#' @format A \code{data.frame} with 3 rows and 2 columns:
#' \describe{
#'   \item{succ}{number of successes}
#'   \item{fail}{number of failures}
#' }
#'
#'
"bb_dat2"


#-------------------------------------------------------------------------------
# qb_dat1

#' Quasi-binomial data (example 1)
#'
#' This data set contains sampled quasi-binomial data from 10 clusters
#' each of size 50. The data set was sampled with \code{rqbinom(n=10, size=50, prob=0.1, phi=3)}.
#'
#' @format A \code{data.frame} with 3 rows and 2 columns:
#' \describe{
#'   \item{succ}{numbers of success}
#'   \item{fail}{numbers of failures}
#' }
#'
#'
"qb_dat1"

#-------------------------------------------------------------------------------
# qb_dat2

#' Quasi-binomial data (example 2)
#'
#' This data set contains sampled quasi binomial data from 3 clusters with
#' different size.The data set was sampled with \code{rqbinom(n=3, size=c(40, 50, 60), prob=0.1, phi=3)}.
#'
#' @format A \code{data.frame} with 3 rows and 2 columns:
#' \describe{
#'   \item{succ}{numbers of success}
#'   \item{fail}{numbers of failures}
#' }
#'
#'
"qb_dat2"


#-------------------------------------------------------------------------------
# qp_dat1

#' Quasi-Poisson data (example 1)
#'
#' This data set contains sampled quasi-Poisson data for 10 clusters.
#' The data set was sampled with \code{rqpois(n=10, lambda=50, phi=3)}.
#'
#' @format A data.frame with two columns
#' \describe{
#'   \item{y}{numbers of eventzs}
#'   \item{offset}{size of experimental units}
#' }
#'
#'
"qp_dat1"

#-------------------------------------------------------------------------------
# qp_dat2

#' Quasi-Poisson data (example 2)
#'
#' This data set contains sampled quasi-Poisson data for 3 clusters.
#' The data set was sampled with \code{rqpois(n=3, lambda=50, phi=3)}.
#'
#' @format A data.frame with two columns
#' \describe{
#'   \item{y}{numbers of eventzs}
#'   \item{offset}{size of experimental units}
#' }
#'
"qp_dat2"



#-------------------------------------------------------------------------------
# c2_dat1

#' Cross-classified data (example 1)
#'
#' c2_dat1 contains data that is sampled from a balanced cross-classified design.
#' This data set is used in order to demonstrate the functionality of the \code{lmer_pi_...()} functions.
#'
#' @format A \code{data.frame} with 27 rows and 3 columns:
#' \describe{
#'   \item{y_ijk}{observations}
#'   \item{a}{treatment a}
#'   \item{b}{treatment b}
#' }
#'
"c2_dat1"


#-------------------------------------------------------------------------------
# c2_dat2

#' Cross-classified data (example 2)
#'
#' c2_dat2 contains data that was sampled from an unbalanced cross-classified design.
#' This data set is used in order to demonstrate the functionality of the \code{lmer_pi_...()} functions.
#'
#' @format A \code{data.frame} with 21 rows and 3 columns:
#' \describe{
#'   \item{y_ijk}{observations}
#'   \item{a}{treatment a}
#'   \item{b}{treatment b}
#' }
#'
#'
"c2_dat2"

#-------------------------------------------------------------------------------
# c2_dat3

#' Cross-classified data (example 3)
#'
#' c2_dat3 contains data that was sampled from a balanced cross-classified design.
#' This data set is used in order to demonstrate the functionality of the \code{lmer_pi_...()} functions.
#'
#' @format A \code{data.frame} with 8 rows and 3 columns:
#' \describe{
#'   \item{y_ijk}{observations}
#'   \item{a}{treatment a}
#'   \item{b}{treatment b}
#' }
#'
#'
"c2_dat3"

#-------------------------------------------------------------------------------
# c2_dat4

#' Cross-classified data (example 4)
#'
#' c2_dat4 contains data that was sampled from an unbalanced cross-classified design.
#' This data set is used in order to demonstrate the functionality of the \code{lmer_pi_...()} functions.
#'
#' @format A \code{data.frame} with 6 rows and 3 columns:
#' \describe{
#'   \item{y_ijk}{observations}
#'   \item{a}{treatment a}
#'   \item{b}{treatment b}
#' }
#'
#'
"c2_dat4"

#-------------------------------------------------------------------------------
#' Historical mortality of male B6C3F1-mice
#'
#' This data set contains historical control data about the mortality of male B6C3F1-mice
#' obtained in long term carcinogenicity studies at the National Toxicology Program
#' presented in NTP Historical Control Reports from 2013 to 2016.
#' It was used in Menssen and Schaarschmidt 2019 as a real life example.
#'
#' @format A \code{data.frame} with 2 rows and 10 columns:
#'  \describe{
#'   \item{dead}{no. of dead mice}
#'   \item{alive}{no. of living mice}
#' }
#'
#' @references
#' Menssen and Schaarschmidt (2019): Prediction intervals for overdispersed binomial
#' data with application to historical controls. Statistics in Medicine.
#' \doi{10.1002/sim.8124} \cr
#' NTP Historical Control Reports: \url{https://ntp.niehs.nih.gov/data/controls}
#'
"mortality_HCD"


#-------------------------------------------------------------------------------
#' Historical numbers of revertant colonies in the Ames test (OECD 471)
#'
#' This data set contains artificial historical control data that was sampled in
#' order to mimic the number of revertant colonies based on two or three petri dishes.
#'
#' @format A \code{data.frame} with 2 rows and 10 columns:
#'  \describe{
#'   \item{rev_col}{no. of revertant colonies}
#'   \item{no_dish}{no. of petri dishes in the control group}
#' }
#'
"ames_HCD"

