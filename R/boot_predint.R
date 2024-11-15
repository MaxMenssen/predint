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
#' of new data from random effects models see \code{\link{lmer_bs}}.
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
#' # Draw 5 bootstrap samples
#' test_boot <- boot_predint(pred_int = test_pi, nboot=50)
#' str(test_boot)
#' summary(test_boot)
#'
#' # Please note that the low number of bootstrap samples was chosen in order to
#' # decrease computing time. For valid analysis draw at least 10000 bootstrap samples.
#'
boot_predint <- function(pred_int, nboot, adjust="within"){

        # Input object needs to be of class predint
        if(!inherits(pred_int, "predint")){
                stop("input object is not of class predint")
        }

        #-----------------------------------------------------------------------
        ### If the PI is quasi Poisson

        if(inherits(pred_int, "quasiPoissonPI")){

                # get the future offsets
                newoffset <- pred_int$newoffset

                # Throw a warning if offsets are different in the new data
                if(length(unique(newoffset)) > 1){
                                warning("offset differs betwen clusters.  bs-calibration is experimental")
                }

                # get the historical offsets
                histoffset <- pred_int$histoffset

                # get the Poisson mean
                lambda <- pred_int$lambda

                # Get the dispersion parameter
                phi <- pred_int$phi

                # pointwise PI or simultaneous PI for several control groups
                if(adjust=="between"){


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

                # Simultaneous PI for complete trial
                if(adjust=="within"){

                        # sampling data to mimic a complete new trial
                        rqpois_within <- function(newoffset, phi, lambda){

                                # get weights for sampling
                                weights_nf <- (newoffset/newoffset[1])[-1]
                                # print(weights_nf)

                                # Calculate kappa based on n_control
                                kappa_c <- (phi-1)/(newoffset[1]*lambda)

                                # Define gamma parameters based on n_control
                                a_c=1/kappa_c
                                b_c=1/(kappa_c*newoffset[1]*lambda)

                                # Sample lambda_control (reflecting n_control)
                                lambda_c <- rgamma(n=1, shape=a_c, rate=b_c)
                                # print(lambda_c)

                                # sample y_control
                                y_c <- rpois(n=1, lambda = lambda_c)

                                # Pointwise prediction
                                if(length(newoffset) == 1){
                                        return(data.frame(y=y_c, offset=newoffset))
                                }

                                # Simultaneous prediction
                                if(length(newoffset) > 1){

                                        # sample y_treatments
                                        y_treat <- vector(length=length(weights_nf))

                                        for(i in 1:length(y_treat)){
                                                y_treat[i] <- rpois(n=1, lambda=weights_nf[i] * lambda_c)
                                        }

                                        return(data.frame(y=c(y_c, y_treat), offset=newoffset))
                                }

                        }


                        # Sampling of future data
                        bs_futdat <- replicate(n=nboot,
                                               rqpois_within(newoffset=newoffset,
                                                             phi=phi,
                                                             lambda=lambda),
                                               simplify = FALSE)
                        # print(bs_futdat)

                        # Sampling of historical data
                        bs_histdat <- replicate(n=nboot,
                                                rqpois(n=length(histoffset),
                                                       lambda=lambda,
                                                       phi=phi,
                                                       offset=histoffset),
                                                simplify = FALSE)

                        # print(bs_histdat)

                        # Define output object
                        out_list <- list(bs_futdat=bs_futdat,
                                         bs_histdat=bs_histdat)

                        # Set class for output object
                        out_s3 <- structure(out_list,
                                            class=c("predint", "bootstrap"))

                        # print(out_s3)
                        return(out_s3)
                }

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



