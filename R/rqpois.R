
#' Sampling of overdispersed Poisson data with constant overdispersion
#'
#' \code{rqpois()} samples overdispersed Poisson data with constant overdispersion from
#' the negative-binomial distribution such that the quasi-Poisson assumption is fulfilled.
#' The following description of the sampling process is based on the parametrization
#' used by Gsteiger et al. 2013.
#'
#' @param n defines the number of clusters (\eqn{I})
#' @param lambda defines the overall Poisson mean (\eqn{\lambda})
#' @param phi dispersion parameter (\eqn{\Phi})
#' @param offset defines the number of experimental units per cluster (\eqn{n_i})
#'
#' @details It is assumed that the dispersion parameter (\eqn{\Phi})
#' is constant for all \eqn{i=1, ... I} clusters, such that the variance becomes
#' \deqn{var(y_i) = \Phi n_i \lambda}
#' For the sampling \eqn{\kappa_i} is defined as
#' \deqn{\kappa_i=(\Phi-1)/(n_i \lambda)}
#' where \eqn{a_i=1/\kappa_i} and \eqn{b_i=1/(\kappa_i n_i \lambda)}. Then, the Poisson means
#' for each cluster are sampled from the gamma distribution
#' \deqn{\lambda_i \sim Gamma(a_i, b_i)}
#' and the observations per cluster are sampled to be
#' \deqn{y_i \sim Pois(\lambda_i).}
#' Please note, that the quasi-Poisson assumption is not in contradiction with the
#' negative-binomial distribution, if the data structure is defined by the number
#' of clusters only (which is the case here) and the offsets are all the same
#' \eqn{n_h = n_{h´} = n}.
#'
#' @return a data.frame containing the sampled observations and the offsets
#'
#' @export
#'
#' @importFrom stats rgamma rpois
#'
#' @references Gsteiger, S., Neuenschwander, B., Mercier, F. and Schmidli, H. (2013):
#' Using historical control information for the design and analysis of clinical
#' trials with overdispersed count data. Statistics in  Medicine, 32: 3609-3622.
#' \doi{10.1002/sim.5851}
#'
#' @examples
#' # set.seed(123)
#' qp_dat1 <- rqpois(n=10, lambda=50, phi=3)
#' qp_dat1
#'
#' # set.seed(123)
#' qp_dat2 <- rqpois(n=3, lambda=50, phi=3)
#' qp_dat2
#'
#'
rqpois <- function(n, lambda, phi, offset=NULL){

        # Phi must be numeric or integer
        if(!(is.numeric(phi) | is.integer(phi))){
                stop("phi must be numeric or integer")
        }

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

        # Phi must be numeric or integer
        if(!(is.numeric(lambda) | is.integer(lambda))){
                stop("lambda must be numeric or integer")
        }

        # lambda must be > 0
        if(lambda<=0){
                stop("lambda must be > 0")
        }

        # If an offset is defined
        if(!is.null(offset)){

                # Offset must be numeric or integer
                if(!(is.numeric(offset) | is.integer(offset))){
                        stop("offset must be numeric or integer")
                }

                # Offset must have the same length as n
                if(length(offset) != n){
                        stop("offset must be of length n")
                }
        }


        #-----------------------------------------------------------------------

        # List with one Poisson mean per cluster
        if(is.null(offset)){
                mu_i <- as.list(rep(x=lambda,
                                      times=n))
        }

        if(!is.null(offset)){
                mu_i <- as.list(lambda*offset)
        }

        # List with kappas
        kappa_fun <- function(x){(phi-1)/x}
        kappa_i <- lapply(X=mu_i,
                        FUN=kappa_fun)


        # Lits with gamma parameters
        abfun <- function(x){1/x}
        a <- lapply(X=kappa_i,
                    FUN=abfun)

        kmu_i <- Map("*", mu_i, kappa_i)
        b <- lapply(X=kmu_i,
                    FUN=abfun)


        # Sampling of the Poisson means from the gamma distribution
        lambda_i <- mapply(FUN=rgamma,
                           shape=a,
                           rate=b,
                           MoreArgs=as.list(1))

        # Sampling of the observations
        y <- unlist(lapply(X=lambda_i, FUN=rpois, n=1))

        # Define the output object
        obs <- data.frame("y"=y)

        if(!is.null(offset)){
                obs$offset <- offset
        }

        if(is.null(offset)){
                obs$offset <- 1
        }

        return(obs)
}

