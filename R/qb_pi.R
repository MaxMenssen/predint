#' Simple uncalibrated prediction intervals for quasi-binomial data
#'
#' @param newsize number of experimental units in the historical clusters.
#' @param histsize number of experimental units in the future clusters.
#' @param pi binomial proportion
#' @param phi dispersion parameter
#' @param q quantile used for interval calculation
#' @param alternative either "both", "upper" or "lower"
#' \code{alternative} specifies, if a prediction interval or
#' an upper or a lower prediction limit should be computed
#' @param histdat additional argument to specivy the historical data set
#' @param newdat additional argument to specivy the actual data set
#' @param algorithm used to define the algorithm for calibration if called via
#' \code{quasi_pois_pi}. This argument is not of interest for the calculation
#' of simple uncalibrated intervals
#'
#' @details This function returns a simple uncalibrated prediction interval
#' \deqn{[l,u]_m = n^*_m \hat{\pi} \pm q \sqrt{n^*_m \hat{\pi} (1- \hat{\pi}) (1+
#' \frac{n^*_m}{\sum_h n_h})}}
#'
#' with \eqn{n^*_m} as the number of experimental units in the \eqn{m=1, 2, ... , M} future clusters,
#' \eqn{\hat{\pi}} as the estimate for the binomial proportion obtained from the
#' historical data, \eqn{\hat{\phi}} as the estimate for the dispersion parameter
#' and \eqn{n_h} as the number of experimental units per historical cluster. \cr
#'
#' The use of this uncalibrated prediction interval is not recommended for practical
#' application.  \cr
#'
#' @return \code{qb_pi} returns an object of class \code{c("predint", "quasiBinomailPI")}.
#' @export
#'
#' @importFrom stats qnorm
#'
#' @examples
#' qb_pi(newsize=50, pi=0.3, phi=3, histsize=c(50, 50, 30), q=qnorm(1-0.05/2))
#'
#' qb_pi(newsize=50, pi=0.3, phi=3, histsize=c(50, 50, 30), q=qnorm(1-0.05), alternative="upper")
qb_pi <- function(newsize,
                  histsize,
                  pi,
                  phi,
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
                if(algorithm != "MS21"){
                        if(algorithm != "MS21mod"){
                                stop("algoritm must be either NULL, MS21 of MS21mod")
                        }
                }
        }

        #-----------------------------------------------------------------------

        # historical number of experimental units
        histn <- sum(histsize)

        # Expected future observations
        y_star_hat <- newsize * pi

        # var for future random variable
        var_y_star <- phi * newsize * pi *(1-pi)

        # var for the expected future observations
        var_y_star_hat <- phi * newsize^2 * pi *(1-pi) / histn

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
                         "phi"=phi,
                         "algorithm"=algorithm)

        out_s3 <- structure(out_list,
                            class=c("predint", "quasiBinomialPI"))

        return(out_s3)
}






