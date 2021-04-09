

#' Sampling of overdispersed binomial data with constant overdispersion
#'
#' rqbinom samples overdispersed binomial data with constant overdispersion from
#' the beta binomial distribution such that the quasi binomial assumption is fulfilled.
#'
#' @param n defines the number of clusters (\eqn{i})
#' @param size integer vector defining the cluster sizes (\eqn{n_i})
#' @param prob overall binomial proportion (\eqn{\pi})
#' @param phi dispersion parameter (\eqn{\Phi})
#'
#' @details It is assumed that the dispersion parameter (\eqn{\Phi})
#' is constant for all \eqn{i=1, ... I} clusters, such that the variance becomes
#' \deqn{\Phi n_i \pi (1-\pi).}
#' For the sampling \eqn{(a+b)_i} is defined as
#' \deqn{(a+b)_i=(\Phi-n_i)/(1-\Phi)}
#' where \eqn{a_i=\pi (a+b)_i} and \eqn{b_i=(a+b)_i-a_i}. Then, the binomial proportions
#' for each cluster are sampled from the beta distribution
#' \deqn{\pi_i \sim Beta(a_i, b_i)}
#' and the numbers of succes for each cluster are sampled to be
#' \deqn{y_i \sim Bin(n_i, \pi_i)}
#'
#' @return
#' @export
#'
#' @examples
#' # Sampling of example data
#' set.seed(345)
#' qb_dat1 <- rqbinom(n=10, size=50, prob=0.1, phi=3)
#' qb_dat1
#'
#' set.seed(345)
#' qb_dat2 <- rqbinom(n=3, size=c(40, 50, 60), prob=0.1, phi=3)
#' qb_dat2
#'
#'
rqbinom <- function(n, size, prob, phi){

        # n must be of length 1
        if(length(n) != 1){
                stop("length(n) must be 1")
        }

        # If phi <= 1 stop
        if(phi<=1){
              stop("phi<=1")
        }

        # clustersize must be >1
        if(min(size)<=1){
                stop("clustersize must be >1")
        }

        # If phi is not smaller than all cluster sizes, stop
        if(min(size) <= phi){
                stop("phi must be < size")
        }

        # All clusters have the same size
        if(length(size)==1){
                nhist <- rep(size, n)
        }

        # Vector of sizes
        else if(length(size) == n){
                nhist <- size
        }

        else{
                stop("length(size) and n do not match")
        }

        # Beta parameters with fixed phi
        asum <- (phi-nhist)/(1-phi)
        a <- prob*asum
        b <- asum-a

        # Sampling proportions from the beta distribution
        pis <- rbeta(n=n, shape1=a, shape2=b)

        # Sampling observations from binomial distribution
        y <- rbinom(n=n, size=nhist, prob=pis)

        # Defining the output data
        x <- nhist-y
        dat <- data.frame(succ=y, fail=x)

        return(dat)
}






