
# n is the numbers of observations to draw
# lambda is the overall poisson mean
# phi is the dispersion parameter

#' Sampling of overdispersed poisson data with constant overdispersion
#'
#' rqpois samples overdispersed poisson data with constant overdispersion from
#' the negative binomial distribution such that the quasi poisson assumption is fulfilled.
#'
#' @param n defines the number of clusters (\eqn{i})
#' @param lambda defines the overall poisson mean (\eqn{\lambda})
#' @param phi dispersion parameter (\eqn{\Phi})
#'
#' @details It is assumed that the dispersion parameter (\eqn{\Phi})
#' is constant for all \eqn{i=1, ... I} clusters, such that the variance becomes
#' \deqn{var(y_i)=\Phi \lambda_i.}
#'
#' @return
#' @export
#'
#' @references Gsteiger, S., Neuenschwander, B., Mercier, F. and Schmidli, H. (2013):
#' Using historical control information for the design and analysis of clinical
#' trials with overdispersed count data. Statist. Med., 32: 3609-3622. https://doi.org/10.1002/sim.5851
#'
#' @examples
rqpois <- function(n, lambda, phi){

        # Phi must be bigger than 1
        if(phi<=1){

                stop("phi<=1")
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







