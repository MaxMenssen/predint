#' Simple uncalibrated prediction intervals for normal distributed data
#'
#' \code{normal_pi()} is a helper function that is internally called by the \code{lmer_pi_...()} functions.
#'  It calculates simple uncalibrated prediction intervals for normal distributed
#'  observations.
#'
#' @param mu overall mean
#' @param pred_se standard error of the prediction
#' @param m number of future observations
#' @param q quantile used for interval calculation
#' @param alternative either "both", "upper" or "lower"
#' \code{alternative} specifies, if a prediction interval or
#' an upper or a lower prediction limit should be computed
#' @param futmat_list used to add the list of future design matrices to the output
#' if called via \code{lmer_pi_futmat()}
#' @param futvec used to add the vector of the historical row numbers that define
#' the future experimental design to the output if called via \code{lmer_pi_futmat()}
#' @param histdat additional argument to specify the historical data set
#' @param newdat additional argument to specify the current data set
#' @param algorithm used to define the algorithm for calibration if called via
#' \code{lmer_pi_...()}. This argument is not of interest for the calculation
#' of simple uncalibrated intervals
#'
#' @details This function returns a simple uncalibrated prediction interval as
#' given in Menssen and Schaarschmidt 2022
#' \deqn{[l,u] = \hat{\mu} \pm q \sqrt{\widehat{var}(\hat{\mu}) + \sum_{c=1}^{C+1} \hat{\sigma}^2_c}}
#'
#' with \eqn{\hat{\mu}} as the expected future observation (historical mean) and
#' \eqn{\hat{\sigma}^2_c} as the \eqn{c=1, 2, ..., C} variance components and \eqn{\hat{\sigma}^2_{C+1}}
#' as the residual variance and \eqn{q} as the quantile used for interval calculation. \cr
#'
#' The direct application of this uncalibrated prediction interval to real life data
#' is not recommended. Please use the \code{lmer_pi_...()} functions for real life applications.  \cr
#'
#' @return \code{normal_pi()} returns an object of class \code{c("predint", "normalPI")}
#' with prediction intervals or limits in the first entry (\code{$prediction}).
#' @export
#'
#' @importFrom stats qnorm
#'
#'@references Menssen and Schaarschmidt (2022): Prediction intervals for all of M future
#' observations based on linear random effects models. Statistica Neerlandica,
#'  \doi{10.1111/stan.12260}
#'
#' @examples
#'
#' # simple PI
#' norm_pred <- normal_pi(mu=10, pred_se=3, m=1)
#' summary(norm_pred)
#'
normal_pi <- function(mu,
                      pred_se,
                      m=1,
                      q=qnorm(1-0.05/2),
                      alternative="both",
                      futmat_list=NULL,
                      futvec=NULL,
                      newdat=NULL,
                      histdat=NULL,
                      algorithm=NULL){

        # mu must be numeric or integer
        if(!(is.numeric(mu) | is.integer(mu))){
                stop("mu must be numeric or integer")
        }

        # pred_se must be numeric or integer
        if(!(is.numeric(pred_se) | is.integer(pred_se))){
                stop("pred_se must be numeric or integer")
        }

        # q must be numeric or integer
        if(!(is.numeric(q) | is.integer(q))){
                stop("q must be numeric or integer")
        }

        if(length(q) > 2){
                stop("length(q) > 2")
        }

        # m must be numeric or integer
        if(!(is.numeric(m) | is.integer(m))){
                stop("m must be numeric or integer")
        }

        # m and numbers of obs in newdat match
        if(!is.null(newdat)){

                if(!is.data.frame(newdat)){
                        if(newdat==1){
                                if(m != 1){
                                        stop("newdat==1 but m!=1")
                                }
                        }
                }


                if(is.data.frame(newdat)){

                        if(nrow(newdat) != m){
                                stop("nrow(newdat) != m")

                        }
                }
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
        ### Calculate the interval


        if(alternative=="both"){

                if(length(q) ==1){
                        lower <- rep(mu, m) - q * rep(pred_se, m)
                        upper <- rep(mu, m) + q * rep(pred_se, m)

                        out <- data.frame(lower,
                                          upper)
                }

                if(length(q) ==2){
                        lower <- rep(mu, m) - q[1] * rep(pred_se, m)
                        upper <- rep(mu, m) + q[2] * rep(pred_se, m)

                        out <- data.frame(lower,
                                          upper)
                }

        }

        if(alternative=="lower"){

                lower <- rep(mu, m) - q * rep(pred_se, m)

                out <- data.frame(lower)
        }

        if(alternative=="upper"){

                upper <- rep(mu, m) + q * rep(pred_se, m)

                out <- data.frame(upper)
        }

        # Output has to be an S3 object
        if(!is.null(newdat)){
                if(!is.data.frame(newdat)){
                        if(newdat==1){
                                newdat <- NULL
                        }
                }


        }
        out_list <- list("prediction"=out,
                         "newdat"=newdat,
                         "futmat_list"=futmat_list,
                         "futvec"=futvec,
                         "histdat"=histdat,
                         "y_star_hat"=rep(mu, m),
                         "pred_se"=rep(pred_se, m),
                         "alternative"=alternative,
                         "q"=q,
                         "mu"=mu,
                         "pred_var"=pred_se^2,
                         "m"=m,
                         "algorithm"=algorithm)

        out_s3 <- structure(out_list,
                            class=c("predint", "normalPI"))

        return(out_s3)
}
