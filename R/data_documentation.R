
#-------------------------------------------------------------------------------
# bb_dat1

#' Beta-binomial data (example 1)
#'
#' This data set contains sampled beta binomial data from 10 clusters
#' each of size 50
#'
#' @format A data frame with 10 rows and 2 variables:
#' \describe{
#'   \item{succ}{number of succes}
#'   \item{fail}{number of failure}
#'   ...
#' }
#'
#' @examples
#' # Sampling bb_dat1
#' set.seed(234)
#' bb_dat1 <- rbbinom(n=10, size=50, prob=0.1, rho=0.06)
#' bb_dat1
#'
#' # Sampling of bb_dat2
#' set.seed(234)
#' bb_dat2 <- rbbinom(n=3, size=c(40, 50, 60), prob=0.1, rho=0.06)
#' bb_dat2
#'
#' # Prediction interval using bb_dat2 as future data
#' beta_bin_pi(histdat=bb_dat1, newdat=bb_dat2)
#'
#' # Upper prediction bound for m=3 future number of successes
#' # that are based on cluster sizes 40, 50, 60 respectively
#' beta_bin_pi(histdat=bb_dat1, newsize=c(40, 50, 60), alternative="upper")
#'
"bb_dat1"

#-------------------------------------------------------------------------------
# bb_dat2

#' Beta-binomial data (example 2)
#'
#' This data set contains sampled beta binomial data from 3 clusters with
#' different size
#'
#' @format A data frame with 3 rows and 2 variables:
#' \describe{
#'   \item{succ}{number of succes}
#'   \item{fail}{number of failure}
#' }
#'
#'
#' @examples
#' # Sampling bb_dat1
#' set.seed(234)
#' bb_dat1 <- rbbinom(n=10, size=50, prob=0.1, rho=0.06)
#' bb_dat1
#'
#' # Sampling of bb_dat2
#' set.seed(234)
#' bb_dat2 <- rbbinom(n=3, size=c(40, 50, 60), prob=0.1, rho=0.06)
#' bb_dat2
#'
#' # Prediction interval using bb_dat2 as future data
#' beta_bin_pi(histdat=bb_dat1, newdat=bb_dat2)
#'
#' # Upper prediction bound for m=3 future number of successes
#' # that are based on cluster sizes 40, 50, 60 respectively
#' beta_bin_pi(histdat=bb_dat1, newsize=c(40, 50, 60), alternative="upper")
#'
"bb_dat2"


#-------------------------------------------------------------------------------
# qb_dat1

#' Quasi-binomial data (example 1)
#'
#' This data set contains sampled quasi-binomial data from from 10 clusters
#' each of size 50
#'
#' @format A data frame with 3 rows and 2 variables:
#' \describe{
#'   \item{succ}{number of succes}
#'   \item{fail}{number of failure}
#' }
#'
#'
#' @examples
#' # Sampling of qb_dat1
#' set.seed(456)
#' qb_dat1 <- rqbinom(n=10, size=50, prob=0.1, phi=3)
#' qb_dat1
#'
#' # Sampling of qb_dat2
#' set.seed(456)
#' qb_dat2 <- rqbinom(n=3, size=c(40, 50, 60), prob=0.1, phi=3)
#' qb_dat2
#'
#' # Prediction interval using qb_dat2 as future data
#' quasi_bin_pi(histdat=qb_dat1, newdat=qb_dat2)
#'
#' # Upper prediction bound for m=3 future observations
#' # that are based on cluster sizes 40, 50, 60 respectively
#' quasi_bin_pi(histdat=qb_dat1, newsize=c(40, 50, 60), alternative="upper")
#'
"qb_dat1"

#-------------------------------------------------------------------------------
# qb_dat2

#' Quasi-binomial data (example 2)
#'
#' This data set contains sampled quasi binomial data from 3 clusters with
#' different size
#'
#' @format A data frame with 3 rows and 2 variables:
#' \describe{
#'   \item{succ}{number of succes}
#'   \item{fail}{number of failure}
#' }
#'
#'
#' @examples
#' # Sampling of qb_dat1
#' set.seed(456)
#' qb_dat1 <- rqbinom(n=10, size=50, prob=0.1, phi=3)
#' qb_dat1
#'
#' # Sampling of qb_dat2
#' set.seed(456)
#' qb_dat2 <- rqbinom(n=3, size=c(40, 50, 60), prob=0.1, phi=3)
#' qb_dat2
#'
#' # Prediction interval using qb_dat2 as future data
#' quasi_bin_pi(histdat=qb_dat1, newdat=qb_dat2)
#'
#' # Upper prediction bound for m=3 future observations
#' # that are based on cluster sizes 40, 50, 60 respectively
#' quasi_bin_pi(histdat=qb_dat1, newsize=c(40, 50, 60), alternative="upper")
#'
"qb_dat2"


#-------------------------------------------------------------------------------
# qp_dat1

#' Quasi-poisson data (example 1)
#'
#' This data set contains sampled quasi-poisson data for 10 clusters
#'
#' @format A vector containing quasi poisson data:
#'
#' @examples
#' set.seed(123)
#' qp_dat1 <- rqpois(n=10, lambda=50, phi=3)
#' qp_dat1
#'
#' set.seed(123)
#' qp_dat2 <- rqpois(n=3, lambda=50, phi=3)
#' qp_dat2
#'
#'#' # Prediction interval using bb_dat2 as future data
#' quasi_pois_pi(histdat=data.frame(qp_dat1),
#'               newdat=data.frame(qp_dat2))
#'
#' # Upper prediction bound for m=3 future observations
#' quasi_pois_pi(histdat=data.frame(qp_dat1),
#'               m=3,
#'               alternative="upper")
#'
"qp_dat1"

#-------------------------------------------------------------------------------
# qp_dat2

#' Quasi-poisson data (example 2)
#'
#' This data set contains sampled quasi-poisson data for 3 clusters
#'
#' @format A vector containing quasi poisson data:
#'
#' @examples
#' set.seed(123)
#' qp_dat1 <- rqpois(n=10, lambda=50, phi=3)
#' qp_dat1
#'
#' set.seed(123)
#' qp_dat2 <- rqpois(n=3, lambda=50, phi=3)
#' qp_dat2
#'
#' # Prediction interval using bb_dat2 as future data
#' quasi_pois_pi(histdat=data.frame(qp_dat1),
#'               newdat=data.frame(qp_dat2))
#'
#' # Upper prediction bound for m=3 future observations
#' quasi_pois_pi(histdat=data.frame(qp_dat1),
#'               m=3,
#'               alternative="upper")
#'
"qp_dat2"
