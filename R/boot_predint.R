#-------------------------------------------------------------------------------
#------- Helper function for bootstrapping from uncalibrated PI ----------------
#-------------------------------------------------------------------------------

#' Bootstrap new data from uncalibrated prediction intervals
#'
#' \code{boot_predint()} is a helper function to bootstrap new data from the simple
#' uncalibrated prediction intervals implemented in predint.
#'
#' @param pred_int object of class \code{c("quasiPoissonPI", "betaBinomialPI",
#' "quasiBinomialPI", negativeBinomialPI)}
#' @param nboot number of bootstraps
#'
#' @details This function only works for binomial and Poisson type data. For the sampling
#' of new data from random effects models see \code{lmer_bs()}.
#'
#' @return \code{boot_predint} returns an object of class \code{c("predint", "bootstrap")}
#' which is a list with two entries: One for bootstrapped historical observations
#' and one for bootstrapped future observations.
#'
#' @export
#'
#' @examples
#' # Simple quasi-Poisson PI
#' test_pi <- qp_pi(histoffset=c(3,3,3,4,5), newoffset=3, lambda=10, phi=3, q=1.96)
#'
#' # Draw 5 bootstrap samles
#' test_boot <- boot_predint(pred_int = test_pi, nboot=50)
#' str(test_boot)
#' summary(test_boot)
#'
#' # Please note that the low number of bootstrap samples was choosen in order to
#' # decrease computing time. For valid analysis draw at least 5000 bootstrap samples.
#'
boot_predint <- function(pred_int, nboot){

        # Input object needs to be of class predint
        if(!inherits(pred_int, "predint")){
                stop("input object is not of class predint")
        }

        #-----------------------------------------------------------------------
        ### If the PI is quasi Poisson

        if(inherits(pred_int, "quasiPoissonPI")){

                # get the future offsets
                newoffset <- pred_int$newoffset

                # get the historical offsets
                histoffset <- pred_int$histoffset

                # get the Poisson mean
                lambda <- pred_int$lambda

                # Get the dispersion parameter
                phi <- pred_int$phi

                # Sampling of future data
                bs_futdat <- replicate(n=nboot,
                                       rqpois(n=length(newoffset),
                                              lambda=lambda,
                                              phi=phi,
                                              offset=newoffset),
                                       simplify = FALSE)


                # Sampling of historical data
                bs_histdat <- replicate(n=nboot,
                                        rqpois(n=length(histoffset),
                                               lambda=lambda,
                                               phi=phi,
                                               offset=histoffset),
                                        simplify = FALSE)

                # Define output object
                out_list <- list(bs_futdat=bs_futdat,
                                 bs_histdat=bs_histdat)

                # Set class for output object
                out_s3 <- structure(out_list,
                                    class=c("predint", "bootstrap"))

                return(out_s3)
        }

        #-----------------------------------------------------------------------
        ### If the PI is negative binomial

        if(inherits(pred_int, "negativeBinomialPI")){

                # get the future offsets
                newoffset <- pred_int$newoffset

                # get the historical offsets
                histoffset <- pred_int$histoffset

                # get the Poisson mean
                lambda <- pred_int$lambda

                # Get the dispersion parameter
                kappa <- pred_int$kappa

                # Sampling of future data
                bs_futdat <- replicate(n=nboot,
                                       rnbinom(n=length(newoffset),
                                              lambda=lambda,
                                              kappa=kappa,
                                              offset=newoffset),
                                       simplify = FALSE)


                # Sampling of historical data
                bs_histdat <- replicate(n=nboot,
                                        rnbinom(n=length(histoffset),
                                               lambda=lambda,
                                               kappa=kappa,
                                               offset=histoffset),
                                        simplify = FALSE)

                # Define output object
                out_list <- list(bs_futdat=bs_futdat,
                                 bs_histdat=bs_histdat)

                # Set class for output object
                out_s3 <- structure(out_list,
                                    class=c("predint", "bootstrap"))

                return(out_s3)
        }

        #-----------------------------------------------------------------------
        ### If the PI is quasi binomial

        if(inherits(pred_int, "quasiBinomialPI")){

                # get the future cluster size
                newsize <- pred_int$newsize

                # get the historical cluster size
                histsize <- pred_int$histsize

                # get the proportion
                pi_hat <- pred_int$pi

                # Get the dispersion parameter
                phi_hat <- pred_int$phi

                # Sampling of future data
                bs_futdat <- replicate(n=nboot,
                                       rqbinom(n=length(newsize),
                                               size=newsize,
                                               prob=pi_hat,
                                               phi=phi_hat),
                                       simplify = FALSE)


                # Sampling of historical data
                bs_histdat <- replicate(n=nboot,
                                        rqbinom(n=length(histsize),
                                                size=histsize,
                                                prob=pi_hat,
                                                phi=phi_hat),
                                        simplify = FALSE)

                # Define output object
                out_list <- list(bs_futdat=bs_futdat,
                                 bs_histdat=bs_histdat)

                # Set class for output object
                out_s3 <- structure(out_list,
                                    class=c("predint", "bootstrap"))

                return(out_s3)
        }

        #-----------------------------------------------------------------------
        ### If the PI is beta binomial

        if(inherits(pred_int, "betaBinomialPI")){

                # get the future cluster size
                newsize <- pred_int$newsize

                # get the historical cluster size
                histsize <- pred_int$histsize

                # get the proportion
                pi_hat <- pred_int$pi

                # Get the dispersion parameter
                rho_hat <- pred_int$rho

                # Sampling of future data
                bs_futdat <- replicate(n=nboot,
                                       rbbinom(n=length(newsize),
                                               size=newsize,
                                               prob=pi_hat,
                                               rho=rho_hat),
                                       simplify = FALSE)


                # Sampling of historical data
                bs_histdat <- replicate(n=nboot,
                                        rbbinom(n=length(histsize),
                                                size=histsize,
                                                prob=pi_hat,
                                                rho=rho_hat),
                                        simplify = FALSE)

                # Define output object
                out_list <- list(bs_futdat=bs_futdat,
                                 bs_histdat=bs_histdat)

                # Set class for output object
                out_s3 <- structure(out_list,
                                    class=c("predint", "bootstrap"))

                return(out_s3)
        }
}



