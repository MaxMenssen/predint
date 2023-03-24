

#' Sampling of negative-binomial data
#'
#'
#'\code{rnbinom()} samples negative-binomial distributed data considering offsets.
#' The following description of the sampling process is based on the parametrization
#' used by Gsteiger et al. 2013.
#'
#' @param n defines the number of clusters (\eqn{I})
#' @param lambda defines the overall Poisson mean (\eqn{\lambda})
#' @param kappa dispersion parameter (\eqn{\kappa})
#' @param offset defines the number of experimental units per cluster (\eqn{n_i})
#'
#' @details In order to sample \eqn{i=1, ... I} observations,
#' the parameters of the gamma distribution are set to \eqn{a=1/\kappa} and
#' \eqn{b_i=1/(\kappa n_i \lambda)}.
#' Then, the Poisson means for each cluster are sampled from the gamma distribution
#' \deqn{\lambda_i \sim Gamma(a_, b_i)}
#' and the observations per cluster are sampled to be
#' \deqn{y_i \sim Pois(\lambda_i).}
#' Please note, that the negative-binomial distribution is not in contradiction with the
#' quasi-Poisson assumption, if the data structure is defined by the number
#' of clusters only (which is the case here) and the offsets are all the same
#' \eqn{n_h = n_{h´} = n}.
#'
#' @return
#' @export
#'
#'#' @references Gsteiger, S., Neuenschwander, B., Mercier, F. and Schmidli, H. (2013):
#' Using historical control information for the design and analysis of clinical
#' trials with overdispersed count data. Statistics in  Medicine, 32: 3609-3622.
#' \doi{10.1002/sim.5851}
#'
#' @examples
#' # No offsets: Same results as rqpois(n=10, lambda=50, phi=3)
#' set.seed(123)
#' predint::rnbinom(n=10, lambda=50, kappa=0.04)
#'
#' # Different offsets
#' set.seed(123)
#' predint::rnbinom(n=3, lambda=50, kappa=0.04, offset=c(3,1,0.5))
#'
#'
rnbinom <- function(n, lambda, kappa, offset=NULL){

        # Phi must be numeric or integer
        if(!(is.numeric(kappa) | is.integer(kappa))){
                stop("phi must be numeric or integer")
        }

        # Phi must be bigger than 1
        if(kappa<=0){

                stop("kappa<=0")
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


        # Lits with gamma parameters
        a <- as.list(rep(1/kappa, n))

        kmu_i <- Map("*", mu_i, kappa)
        b <- lapply(X=kmu_i,
                    FUN=function(x){1/x})


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








