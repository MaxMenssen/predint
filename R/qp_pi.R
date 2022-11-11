
#' Simple uncalibrated prediction intervals for quasi-Poisson data
#'
#' \code{qp_pi()} is a helper fuction that is called in \code{quasi_pois_pi()}. It
#' calculates simple uncalibrated prediction intervals for Poisson
#' data with constant overdispersion (quasi-Poisson).
#'
#' @param histoffset number of experimental units in the historical clusters.
#' @param newoffset number of experimental units in the future clusters.
#' @param lambda overall Poisson mean.
#' @param phi dispersion parameter
#' @param q quantile used for interval calculation
#' @param alternative either "both", "upper" or "lower".
#' \code{alternative} specifies, if a prediction interval or
#' an upper or a lower prediction limit should be computed.
#' @param histdat additional argument to specivy the historical data set
#' @param newdat additional argument to specivy the actual data set
#'
#' @details This function returns a simple uncalibrated prediction interval
#' \deqn{[l,u] = n^*_m \hat{\lambda} \pm q \sqrt{n^*_m \hat{\phi} \hat{\lambda} +
#' n^{*2}_m \hat{\phi} \hat{\lambda} \frac{1}{\sum_h n_h}}}
#'
#' with \eqn{n^*_m} as the number of experimental units in the future clusters,
#' \eqn{\hat{\lambda}} as the estimate for the Poisson mean obtained from the
#' historical data, \eqn{\hat{\phi}} as the estimate for the dispersion parameter
#' and \eqn{n_h} as the number of experimental units per historical cluster. \cr
#'
#' The use of this uncalibrated prediction interval is not recommended for practical
#' application.  \cr
#'
#' @return \code{qp_pi} returns an object of class \code{c("predint", "quasiPoissonPI")}.
#'
#' @export
#'
#' @importFrom stats qnorm
#'
#' @examples
#' # Prediction interval
#' qp_pi(newoffset=c(3), lambda=3, ph=3, histoffset=1:9, q=qnorm(1-0.05/2))
#'
#' # Upper prediction border
#' qp_pi(newoffset=c(1), lambda=3, ph=3, histoffset=1:9, q=qnorm(1-0.05), alternative="upper")
#'
#'
qp_pi <- function(newoffset,
                  histoffset,
                  lambda,
                  phi,
                  q=qnorm(1-0.05/2),
                  alternative="both",
                  newdat=NULL,
                  histdat=NULL){

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

        # Phi must be bigger than 1
        if(phi<=1){

                stop("phi<=1")
        }

        # # histn must be one number
        # if(length(histn) != 1){
        #         stop("length(histn) != 1")
        # }
        #
        # # histn must be numeric or integer
        # if(!(is.numeric(histn) | is.integer(histn))){
        #         stop("histn must be numeric or integer")
        # }

        # q must be numeric or integer
        if(!(is.numeric(q) | is.integer(q))){
                stop("q must be numeric or integer")
        }

        # alternative must be defined
        if(isTRUE(alternative!="both" && alternative!="lower" && alternative!="upper")){
                stop("alternative must be either both, lower or upper")
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

                lower <- y_star_hat - q * pred_se
                upper <- y_star_hat + q * pred_se

                out <- data.frame(lower,
                                  upper)
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
                         "q"=q,
                         "lambda"=lambda,
                         "phi"=phi)

        out_s3 <- structure(out_list,
                            class=c("predint", "quasiPoissonPI"))

        return(out_s3)
}






