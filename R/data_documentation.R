
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
#' @examples
#' # Upper prediction limit for m=3 future number of successes
#' # that are based on cluster sizes 40, 50, 60 respectively
#' beta_bin_pi(histdat=bb_dat1, newsize=c(40, 50, 60), alternative="upper", nboot=100)
#'
#' # Please note that nboot was set to 100 in order to decrease computing time
#' # of the example. For a valid analysis set nboot=10000.
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
#' @examples
#' # Prediction interval using bb_dat2 as future data
#' beta_bin_pi(histdat=bb_dat1, newdat=bb_dat2, nboot=100)
#'
#' # Please note that nboot was set to 100 in order to decrease computing time
#' # of the example. For a valid analysis set nboot=10000.
#'
"bb_dat2"


#-------------------------------------------------------------------------------
# qb_dat1

#' Quasi-binomial data (example 1)
#'
#' This data set contains sampled quasi-binomial data from from 10 clusters
#' each of size 50. The data set was sampled with \code{rqbinom(n=10, size=50, prob=0.1, phi=3)}.
#'
#' @format A \code{data.frame} with 3 rows and 2 columns:
#' \describe{
#'   \item{succ}{numbers of success}
#'   \item{fail}{numbers of failur}
#' }
#'
#'
#' @examples
#' # Upper prediction limit for m=3 future observations
#' # that are based on cluster sizes 40, 50, 60 respectively
#' quasi_bin_pi(histdat=qb_dat1, newsize=c(40, 50, 60), alternative="upper", nboot=100)
#'
#' # Please note that nboot was set to 100 in order to decrease computing time
#' # of the example. For a valid analysis set nboot=10000.
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
#'   \item{fail}{numbers of failure}
#' }
#'
#'
#' @examples
#' # Prediction interval using qb_dat2 as future data
#' quasi_bin_pi(histdat=qb_dat1, newdat=qb_dat2, nboot=100)
#'
#' # Please note that nboot was set to 100 in order to decrease computing time
#' # of the example. For a valid analysis set nboot=10000.
#'
"qb_dat2"


#-------------------------------------------------------------------------------
# qp_dat1

#' Quasi-Poisson data (example 1)
#'
#' This data set contains sampled quasi-poisson data for 10 clusters.
#' The data set was sampled with \code{rqpois(n=10, lambda=50, phi=3)}.
#'
#' @format An integer vector with ten entries containing quasi-Poisson data
#'
#' @examples
#' # Upper prediction limit for m=3 future observations
#' quasi_pois_pi(histdat=data.frame(qp_dat1), m=3, alternative="upper", nboot=100)
#'
#' # Please note that nboot was set to 100 in order to decrease computing time
#' # of the example. For a valid analysis set nboot=10000.
#'
"qp_dat1"

#-------------------------------------------------------------------------------
# qp_dat2

#' Quasi-Poisson data (example 2)
#'
#' This data set contains sampled quasi-poisson data for 3 clusters.
#' The data set was sampled with \code{rqpois(n=3, lambda=50, phi=3)}.
#'
#' @format An integer vector with three entries containing quasi-Poisson data
#'
#' @examples
#' # Prediction interval using qp_dat2 as future data
#' quasi_pois_pi(histdat=data.frame(qp_dat1), newdat=data.frame(qp_dat2), nboot=100)
#'
#' # Please note that nboot was set to 100 in order to decrease computing time
#' # of the example. For a valid analysis set nboot=10000.
#'
"qp_dat2"



#-------------------------------------------------------------------------------
# c2_dat1

#' Cross-classified data (example 1)
#'
#' c2_dat1 contains data that is sampled from a balanced cross-classified design.
#'
#' @format A \code{data.frame} with 27 rows and 3 columns:
#' \describe{
#'   \item{y_ijk}{observations}
#'   \item{a}{treatment a}
#'   \item{b}{treatment b}
#' }
#'
#' @examples
#' # loading lme4
#' library(lme4)
#'
#' # Fitting a random effects model based on c2_dat_1
#' fit <- lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1)
#' summary(fit)
#'
#' # Upper prediction limit for m=3 future observations
#' \donttest{lmer_pi_unstruc(model=fit, m=3, alternative="upper", nboot=100)}
#'
#' # Please note that nboot was set to 100 in order to decrease computing time
#' # of the example. For a valid analysis set nboot=10000.
#'
"c2_dat1"


#-------------------------------------------------------------------------------
# c2_dat2

#' Cross-classified data (example 2)
#'
#' c2_dat2 contains data that was sampled from an unbalanced cross-classified design.
#'
#' @format A \code{data.frame} with 21 rows and 3 columns:
#' \describe{
#'   \item{y_ijk}{observations}
#'   \item{a}{treatment a}
#'   \item{b}{treatment b}
#' }
#'
#' @examples
#' # loading lme4
#' library(lme4)
#'
#' # Fitting a random effects model based on c2_dat_1
#' fit <- lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1)
#' summary(fit)
#'
#' # Prediction interval using c2_dat2 as future data
#' \donttest{lmer_pi_unstruc(model=fit, newdat=c2_dat2, alternative="both", nboot=100)}
#'
#' # Please note that nboot was set to 100 in order to decrease computing time
#' # of the example. For a valid analysis set nboot=10000.
#'
"c2_dat2"

#-------------------------------------------------------------------------------
# c2_dat3

#' Cross-classified data (example 3)
#'
#' c2_dat3 contains data that was sampled from a balanced cross-classified design.
#'
#' @format A \code{data.frame} with 8 rows and 3 columns:
#' \describe{
#'   \item{y_ijk}{observations}
#'   \item{a}{treatment a}
#'   \item{b}{treatment b}
#' }
#'
#' @examples
#'
#' # loading lme4
#' library(lme4)
#'
#' # Fitting a random effects model based on c2_dat_1
#' fit <- lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1)
#' summary(fit)
#'
#' #----------------------------------------------------------------------------
#'
#' ### Prediction interval using c2_dat3 as future data
#' # without printing c2_dat3 in the output
#' \donttest{
#' # Row numbers of the historical data c2_dat1 that define the structure of
#' # the future data c2_dat3
#' futvec <- c(1, 2, 4, 5, 10, 11, 13, 14)
#'
#' # Calculating the PI
#' lmer_pi_futvec(model=fit, futvec=futvec, alternative="both", nboot=100)}
#'
#' # Please note that nboot was set to 100 in order to decrease computing time
#' # of the example. For a valid analysis set nboot=10000.
#'
"c2_dat3"

#-------------------------------------------------------------------------------
# c2_dat4

#' Cross-classified data (example 4)
#'
#' c2_dat4 contains data that was sampled from an unbalanced cross-classified design.
#'
#' @format A \code{data.frame} with 6 rows and 3 columns:
#' \describe{
#'   \item{y_ijk}{observations}
#'   \item{a}{treatment a}
#'   \item{b}{treatment b}
#' }
#'
#' @examples
#'
#' # loading lme4
#' library(lme4)
#'
#' # Fitting a random effects model based on c2_dat_1
#' fit <- lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1)
#' summary(fit)
#'
#' #----------------------------------------------------------------------------
#'
#' ### Prediction interval using c2_dat4 as future data
#'
#' # c2_dat4 has no replication for b. Hence the list of design matrices can not be
#' # generated by lme4::lFormula() and has to be provided by hand via futmat_list.
#'
#' c2_dat4
#'
#' # Build a list containing the design matrices
#'
#' fml <- vector(length=4, "list")
#'
#' names(fml) <- c("a:b", "b", "a", "Residual")
#'
#' fml[["a:b"]] <- matrix(nrow=6, ncol=2, data=c(1,1,0,0,0,0, 0,0,1,1,1,1))
#'
#' fml[["b"]] <- matrix(nrow=6, ncol=1, data=c(1,1,1,1,1,1))
#'
#' fml[["a"]] <- matrix(nrow=6, ncol=2, data=c(1,1,0,0,0,0, 0,0,1,1,1,1))
#'
#' fml[["Residual"]] <- diag(6)
#'
#' fml
#'
#' # Please note, that the design matrix for the interaction term a:b is also
#' # provided even there is no replication for b, since it is believed that
#' # both, the historical and the future data descent from the same data generating
#' # process.
#'
#' # Calculate the PI
#' lmer_pi_futmat(model=fit, futmat_list=fml, alternative="both", nboot=100)
#'
#' # Please note that nboot was set to 100 in order to decrease computing time
#' # of the example. For a valid analysis set nboot=10000.
#'
"c2_dat4"


