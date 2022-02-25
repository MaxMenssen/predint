
#' Sampling of overdispersed poisson data with constant overdispersion
#'
#' rqpois samples overdispersed poisson data with constant overdispersion from
#' the negative-binomial distribution such that the quasi-poisson assumption is fulfilled.
#' The following description of the sampling process is based on the parametrization
#' used by Gsteiger et al. 2013.
#'
#' @param n defines the number of clusters (\eqn{i})
#' @param lambda defines the overall poisson mean (\eqn{\lambda})
#' @param phi dispersion parameter (\eqn{\Phi})
#'
#' @details It is assumed that the dispersion parameter (\eqn{\Phi})
#' is constant for all \eqn{i=1, ... I} clusters, such that the variance becomes
#' \deqn{var(y_i)= \lambda(1+\lambda \kappa) = \Phi \lambda.}
#' For the sampling \eqn{\kappa} is defined as
#' \deqn{\kappa=(\Phi-1)/(\lambda)}
#' where \eqn{a=1/\kappa} and \eqn{b=1/(\kappa \lambda)}. Then, the poisson means
#' for each cluster are sampled from the gamma distribution
#' \deqn{\lambda_i \sim Gamma(a, b)}
#' and the observations per cluster are sampled to be
#' \deqn{y_i \sim Pois(\lambda_i).}
#' Please note, that the quasi-poisson assumption is not in contradiction with the
#' negative-binomial distribution if the data structure is defined by the number
#' of clusters only (which is the case here), rather than by a complex randomization structure.
#'
#' @return a vector containing the sampled observations (one per cluster)
#'
#' @export
#'
#'@importFrom stats rgamma rpois
#'
#' @references Gsteiger, S., Neuenschwander, B., Mercier, F. and Schmidli, H. (2013):
#' Using historical control information for the design and analysis of clinical
#' trials with overdispersed count data. Statist. Med., 32: 3609-3622.
#' \url{https://doi.org/10.1002/sim.5851}
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
#'
rqpois <- function(n, lambda, phi){

        # Phi must be bigger than 1
        if(phi<=1){

                stop("phi<=1")
        }

        # n must be integer
        if(!isTRUE(all(n == floor(n)))){
                stop("'n' must be an integer")
        }

        # n must be of length 1
        if(length(n) != 1){
                stop("length(n) must be 1")
        }

        # lambda must be > 0
        if(lambda<=0){
                stop("lambda must be > 0")
        }


        # Defining kappa following the parametrisation of the nb-distribution
        # of Gsteiger et al 2013 (Stats in Med)
        kappa <- (phi-1)/lambda

        # Gamma parameters
        a <- 1/kappa
        b <- 1/(kappa*lambda)

        lambda_h <- rgamma(n=n, shape=a, rate = b)

        obs <- integer(length(lambda_h))

        for(h in 1:length(obs)){
                obs[h] <- rpois(n=1, lambda=lambda_h[h])
        }

        return(obs)
}







