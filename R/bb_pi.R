#' Simple uncalibrated prediction intervals for beta-binomial data
#'
#' \code{bb_pi()} is a helper function that is internally called by \code{beta_bin_pi()}. It
#' calculates simple uncalibrated prediction intervals for binary
#' data with overdispersion changing between the clusters (beta-binomial).
#'
#' @param newsize number of experimental units in the historical clusters
#' @param histsize number of experimental units in the future clusters
#' @param pi binomial proportion
#' @param rho intra class correlation
#' @param q quantile used for interval calculation
#' @param alternative either "both", "upper" or "lower"
#' \code{alternative} specifies, if a prediction interval or
#' an upper or a lower prediction limit should be computed
#' @param histdat additional argument to specify the historical data set
#' @param newdat additional argument to specify the current data set
#' @param algorithm used to define the algorithm for calibration if called via
#' \code{beta_bin_pi()}. This argument is not of interest for the calculation
#' of simple uncalibrated intervals
#'
#' @details This function returns a simple uncalibrated prediction interval
#' \deqn{[l,u]_m = n^*_m \hat{\pi} \pm q \sqrt{n^*_m \hat{\pi} (1- \hat{\pi}) [1 + (n^*_m -1) \hat{\rho}] +
#' [\frac{n^{*2}_m \hat{\pi} (1- \hat{\pi})}{\sum_h n_h} + \frac{\sum_h n_h -1}{\sum_h n_h} n^{*2}_m \hat{\pi} (1- \hat{\pi}) \hat{\rho}]}}
#'
#' with \eqn{n^*_m} as the number of experimental units in the \eqn{m=1, 2, ... , M} future clusters,
#' \eqn{\hat{\pi}} as the estimate for the binomial proportion obtained from the
#' historical data, \eqn{\hat{\rho}} as the estimate for the intra class correlation
#' and \eqn{n_h} as the number of experimental units per historical cluster. \cr
#'
#' The direct application of this uncalibrated prediction interval to real life data
#' is not recommended. Please use \code{beta_bin_pi()} for real life applications.  \cr
#'
#' @return \code{bb_pi()} returns an object of class \code{c("predint", "betaBinomialPI")}
#' with prediction intervals or limits in the first entry (\code{$prediction}).
#' @export
#'
#' @examples
#' # Pointwise uncalibrated PI
#' bb_pred <- bb_pi(newsize=c(50), pi=0.3, rho=0.05, histsize=rep(50, 20), q=qnorm(1-0.05/2))
#' summary(bb_pred)
#'
bb_pi <- function(newsize,
                  histsize,
                  pi,
                  rho,
                  q=qnorm(1-0.05/2),
                  alternative="both",
                  newdat=NULL,
                  histdat=NULL,
                  algorithm=NULL){

        # histsize must be numeric or integer
        if(!(is.numeric(histsize) | is.integer(histsize))){
                stop("histsize must be numeric or integer")
        }

        # newsize must be numeric or integer
        if(!(is.numeric(newsize) | is.integer(newsize))){
                stop("newsize must be numeric or integer")
        }

        # pi must be be smaller than 1 and bigger than
        if(pi<0){
                stop("pi must be bigger than 0 and smaller than 1")
        }

        if(pi>1){
                stop("pi must be bigger than 0 and smaller than 1")
        }

        # rho must be numeric or integer
        if(!(is.numeric(rho) | is.integer(rho))){
                stop("rho must be numeric or integer")
        }

        # rho must be bigger than 0
        if(rho<0){
                stop("rho must be bigger than 0")
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
        histn <- sum(histsize)

        # Expected future observations
        y_star_hat <- newsize * pi

        # var for future random variable
        var_y_star <-  newsize * pi * (1-pi) * (1 + (newsize - 1) * rho)

        # var for the expected future observations
        var_y_star_hat <-  (newsize^2 * pi * (1-pi)) / histn +
                (histn-1) / histn * newsize^2 * pi * (1-pi) * rho

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
                         "newsize"=newsize,
                         "newdat"=newdat,
                         "histsize"=histsize,
                         "histdat"=histdat,
                         "y_star_hat"=y_star_hat,
                         "pred_se"=pred_se,
                         "alternative"=alternative,
                         "q"=q,
                         "pi"=pi,
                         "rho"=rho,
                         "algorithm"=algorithm)

        out_s3 <- structure(out_list,
                            class=c("predint", "betaBinomialPI"))

        return(out_s3)
}



