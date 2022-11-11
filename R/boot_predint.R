#-------------------------------------------------------------------------------
#------- Helper function for bootstrapping from uncalibrated PI ----------------
#-------------------------------------------------------------------------------

#' Bootstrap new data from uncalibrated prediction intervals
#'
#' \code{boot_predint} is a helper function to bootstrap new data from the simple
#' uncalibrated prediction intervals implemented in predint
#'
#' @param pred_int simple prediction interval of class \code{c("quasiPoissonPI")}
#' @param nboot number of bootstraps
#'
#' @return \code{boot_predint} returns an object of class \code{c("predint", "bootstrap")}
#' which is a list with two entries: One for bootstrapped historical observations
#' and one for bootstrapped future observations.
#'
#' @export
#'
#' @examples
#' test_pi <- qp_pi(histoffset=c(1:9), newoffset=c(3), lambda=10, phi=3, q=1.96, alternative="both")
#'
#' test_boot <- boot_predint(pred_int = test_pi, nboot=5)
#' test_boot
#'
boot_predint <- function(pred_int, nboot){

        # Input object needs to be of class predint
        if(!inherits(pred_int, "predint")){
                stop("input object is not of class predint")
        }

        # If the PI is quasi Poisson
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
}


# test_pi <- qp_pi(histoffset=c(1:9),
#                  newoffset=c(2,3),
#                  lambda=10,
#                  phi=3,
#                  q=qnorm(1-0.05/2),
#                  alternative="both")
# test_pi
#
# test_boot <- boot_predint(pred_int = test_pi,
#                           nboot=5)
# test_boot$bs_futdat
# test_boot$bs_histdat





