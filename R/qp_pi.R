
#' Simple uncalibrated prediction intervals for quasi-Poisson data
#'
#' \code{qp_pi()} is a helper function that is internally called by  \code{quasi_pois_pi()}. It
#' calculates simple uncalibrated prediction intervals for Poisson
#' data with constant overdispersion (quasi-Poisson assumption).
#'
#' @param histoffset number of experimental units in the historical clusters
#' @param newoffset number of experimental units in the future clusters
#' @param lambda overall Poisson mean
#' @param phi dispersion parameter
#' @param q quantile used for interval calculation
#' @param alternative either "both", "upper" or "lower"
#' \code{alternative} specifies, if a prediction interval or
#' an upper or a lower prediction limit should be computed
#' @param adjust only important if called via \code{quasi_pois_pi()}
#' @param histdat additional argument to specify the historical data set
#' @param newdat additional argument to specify the current data set
#' @param algorithm used to define the algorithm for calibration if called via
#' \code{quasi_pois_pi()}. This argument is not of interest for the calculation
#' of simple uncalibrated intervals
#'
#' @details This function returns a simple uncalibrated prediction interval
#' \deqn{[l,u]_m = n^*_m \hat{\lambda} \pm q \sqrt{n^*_m \hat{\phi} \hat{\lambda} +
#'  \frac{n^{*2}_m \hat{\phi} \hat{\lambda}}{\sum_h n_h}}}
#'
#' with \eqn{n^*_m} as the number of experimental units in the \eqn{m=1, 2, ... , M} future clusters,
#' \eqn{\hat{\lambda}} as the estimate for the Poisson mean obtained from the
#' historical data, \eqn{\hat{\phi}} as the estimate for the dispersion parameter
#' and \eqn{n_h} as the number of experimental units per historical cluster. \cr
#'
#' The direct application of this uncalibrated prediction interval to real life data
#' is not recommended. Please use the \code{quasi_pois_pi_pi()} functions for real life applications.
#'
#' @return \code{qp_pi} returns an object of class \code{c("predint", "quasiPoissonPI")}.
#'
#' @export
#'
#' @importFrom stats qnorm
#'
#' @examples
#' # Prediction interval
#' qp_pred <- qp_pi(newoffset=3, lambda=3, phi=3, histoffset=1:9, q=qnorm(1-0.05/2))
#' summary(qp_pred)
#'
qp_pi <- function(newoffset,
                  histoffset,
                  lambda,
                  phi,
                  q=qnorm(1-0.05/2),
                  alternative="both",
                  adjust=NULL,
                  newdat=NULL,
                  histdat=NULL,
                  algorithm=NULL){

        # histoffset must be numeric or integer
        if(!(is.numeric(histoffset) | is.integer(histoffset))){
                stop("histoffset must be numeric or integer")
        }

        # newoffset must be numeric or integer
        if(!(is.numeric(newoffset) | is.integer(newoffset))){
                stop("newoffset must be numeric or integer")
        }

        # lambda must be numeric or integer
        if(!(is.numeric(lambda) | is.integer(lambda))){
                stop("lambda must be numeric or integer")
        }

        # Phi must be numeric or integer
        if(!(is.numeric(phi) | is.integer(phi))){
                stop("phi must be numeric or integer")
        }

        # q must be numeric or integer
        if(!(is.numeric(q) | is.integer(q))){
                stop("q must be numeric or integer")
        }

        if(length(q) > 2){
                stop("length(q) > 2")
        }

        # alternative must be defined
        if(isTRUE(alternative!="both" && alternative!="lower" && alternative!="upper")){
                stop("alternative must be either both, lower or upper")
        }

        # check algorithm
        if(!is.null(algorithm)){
                if(algorithm != "MS22"){
                        if(algorithm != "MS22mod"){
                                stop("algoritm must be either NULL, MS22 of MS22mod")
                        }
                }
        }

        #-----------------------------------------------------------------------

        # historical number of experimental units
        histn <- sum(histoffset)

        # Expected future observations
        y_star_hat <- newoffset * lambda

        # var for future random variable
        var_y_star <- newoffset * lambda * phi

        # var for the expected future observations
        var_y_star_hat <- newoffset^2 * lambda * phi * 1/histn

        # SE for prediction
        pred_se <- sqrt(var_y_star + var_y_star_hat)

        #-----------------------------------------------------------------------
        ### Calculate the interval

        if(alternative=="both"){

                if(length(q) ==1){
                        lower <- y_star_hat - q * pred_se
                        upper <- y_star_hat + q * pred_se

                        out <- data.frame(lower,
                                          upper)
                }

                if(length(q) ==2){
                        lower <- y_star_hat - q[1] * pred_se
                        upper <- y_star_hat + q[2] * pred_se

                        out <- data.frame(lower,
                                          upper)
                }

        }

        if(alternative=="lower"){

                lower <- y_star_hat - q * pred_se

                out <- data.frame(lower)
        }

        if(alternative=="upper"){

                upper <- y_star_hat + q * pred_se

                out <- data.frame(upper)
        }

        # Output has to be an S3 object

        out_list <- list("prediction"=out,
                         "newoffset"=newoffset,
                         "newdat"=newdat,
                         "histoffset"=histoffset,
                         "histdat"=histdat,
                         "y_star_hat"=y_star_hat,
                         "pred_se"=pred_se,
                         "alternative"=alternative,
                         "adjust"=adjust,
                         "q"=q,
                         "lambda"=lambda,
                         "phi"=phi,
                         "algorithm"=algorithm)

        out_s3 <- structure(out_list,
                            class=c("predint", "quasiPoissonPI"))

        return(out_s3)
}






