

#' Sampling of beta-binomial data
#'
#' \code{rbbinom()} samples beta-binomial data according to Menssen and Schaarschmidt (2019).
#'
#' @param n defines the number of clusters (\eqn{i})
#' @param size integer vector defining the number of trials per cluster (\eqn{n_i})
#' @param prob probability of success on each trial (\eqn{\pi})
#' @param rho intra class correlation (\eqn{\rho})
#'
#' @details For beta binomial data with \eqn{i=1, ... I} clusters, the variance is
#' \deqn{var(y_i)= n_i \pi (1-\pi) [1+ (n_i - 1) \rho]}
#' with \eqn{\rho} as the intra class correlation coefficient
#' \deqn{\rho = 1 / (1+a+b).}
#' For the sampling \eqn{(a+b)} is defined as
#' \deqn{(a+b)=(1-\rho)/\rho}
#' where \eqn{a=\pi (a+b)} and \eqn{b=(a+b)-a}. Then, the binomial proportions
#' for each cluster are sampled from the beta distribution
#' \deqn{\pi_i \sim Beta(a, b)}
#' and the number of successes for each cluster are sampled to be
#' \deqn{y_i \sim Bin(n_i, \pi_i).}
#' In this parametrization \eqn{E(\pi_i)=\pi=a/(a+b)} and \eqn{E(y_i)=n_i \pi}.
#' Please note, that \eqn{1+ (n_i-1) \rho} is a constant if all cluster sizes are
#' the same and hence, in this special case, also the quasi-binomial assumption is
#' fulfilled.
#'
#' @return a \code{data.frame} with two columns (succ, fail)
#'
#' @export
#'
#' @importFrom stats rbeta rbinom
#'
#' @references
#' Menssen M, Schaarschmidt F.: Prediction intervals for overdispersed binomial data
#' with application to historical controls. Statistics in Medicine. 2019;38:2652-2663.
#' \doi{10.1002/sim.8124}
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

        # n must be integer
        if(!isTRUE(all(n == floor(n)))){
                stop("'n' must be an integer")
        }

        # n must be of length 1
        if(length(n) != 1){
                stop("length(n) must be 1")
        }

        # size must be integer
        if(!isTRUE(all(size == floor(size)))){
                stop("'size' must contain integer values only")
        }

        # Prob must be element of [0,1]
        if(prob < 0 ){
                stop("prob is not element of [0,1]")
        }

        # Prob must be element of [0,1]
        if(prob > 1 ){
                stop("prob is not element of [0,1]")
        }


        # Rho must be bigger than 0
        if(rho<=0){
                stop("rho <= 0")
        }

        # Rho must be smaller than 1
        if(rho>=1){
                stop("rho > 1")
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









