
#' Sampling of negative binomial data
#'
#' \code{rnbinom()} samples negative-binomial data.
#' The following description of the sampling process is based on the parametrization
#' used by Gsteiger et al. 2013.
#'
#' @param n defines the number of clusters (\eqn{I})
#' @param lambda defines the overall Poisson mean (\eqn{\lambda})
#' @param kappa dispersion parameter (\eqn{\kappa})
#' @param offset defines the number of experimental units per cluster (\eqn{n_i})
#'
#' @details
#' Observations that descent from the negative-binomial distribution have variance
#' \deqn{var(Y_i) = n_i \lambda (1+ \kappa n_i \lambda).}
#' Negative-biomial observations can be sampled based on predefined values of \eqn{\kappa},
#' \eqn{\lambda} and \eqn{n_i}: \cr
#' Define the parameters of the gamma distribution as \eqn{a=\frac{1}{\kappa}} and
#' \eqn{b_i=\frac{1}{\kappa n_i \lambda}}. Then, sample the Poisson means for each cluster
#' \deqn{\lambda_i \sim Gamma(a, b_i).}
#' Finally, the observations \engn{y_i} are sampled from the Poisson distribution
#' \deqn{y_i \sim Pois(\lambda_i)}
#'
#' @return
#' @export
#'
#' @examples
#' # coming soon
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








