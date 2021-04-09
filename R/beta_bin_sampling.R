

#' Sampling of beta binomial data
#'
#' rbbinom samples beta binomial data according to Menssen and Schaarschmidt (2019)
#'
#' @param n defines the number of clusters (\eqn{i})
#' @param size integer vector defining the cluster sizes (\eqn{n_i})
#' @param prob overall binomial proportion (\eqn{\pi})
#' @param rho intra class correlation (\eqn{\rho})
#'
#' @details The sampling is done such that
#' \deqn{x_i \sim Bin(n_i, \pi_i) and \bi_i \sim Beta(a,b)}
#' with \eqn{i=1..I} clusters where \eqn{E(\pi_i)=\pi=a/(a+b)} and
#' \eqn{E(x_i)=n_i \pi}. Hence, the beta binomial variance is
#' \deqn{var(x_i)=n_i \pi (1-\pi) (1+ (n_i-1) \rho)}
#' with \eqn{\rho} as the intra class correlation
#' \deqn{\rho = \frac{1}{1+a+b}}
#' Please note, that \eqn{1+ (n_i-1) \rho} is a constant if all cluster sizes are
#' the same and hence, in this special case, also the quasi binomial assumption is
#' fulfilled.
#'
#' @return a data frame with two columns (succ, fail)
#' @export
#'
#' @references
#' Menssen M, Schaarschmidt F.: Prediction intervals for overdispersed binomial data
#' with application to historical controls. Statistics in Medicine. 2019;38:2652-2663.
#' https://doi.org/10.1002/sim.8124
#'
#' @examples
#' # Sampling of example data
#' set.seed(234)
#' bb_dat1 <- rbbinom(n=10, size=50, prob=0.1, rho=0.06)
#' bb_dat1
#'
#'
#' set.seed(234)
#' bb_dat2 <- rbbinom(n=3, size=c(40, 50, 60), prob=0.1, rho=0.06)
#' bb_dat2
#'
#'
rbbinom <- function(n, size, prob, rho){

        # n must be of length 1
        if(length(n) != 1){
                stop("length(n) must be 1")
        }

        # Rho must be bigger than 0
        if(rho<=0){
                stop("rho<=0")
        }

        # Same clustersise for all clusters (one size given)
        if(length(size)==1){
                nhist <- rep(size, n)
        }

        # Severeal cluster sizes
        else if(length(size) == n){
                nhist <- size
        }

        # If size and n do not match, stop
        else{
                stop("length(size) and n do not match")
        }


        # Beta parameters with fixed rho and asum
        asum <-(1-rho)/rho
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

#------------------------------------------------------------------------------









