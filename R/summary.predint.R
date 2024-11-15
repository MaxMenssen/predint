



#' Summarizing objects of class \code{predint}
#'
#' This function gives a summary about the prediction intervals (and limits)
#' computed with \pkg{predint}.
#'
#' @param object object of class \code{predint}
#' @param ... further arguments passed over to \code{base::cbind()} and \code{base::data.frame()}
#'
#' @return A \code{data.frame} containing the current data (if provided via \code{newdat}),
#' the prediction interval (or limit), the expected value for the future observation,
#' the bootstrap calibrated coefficient(s), the prediction standard error and
#' a statement about the coverage for each future observation, if new observations
#' were provided via \code{newdat}.
#'
#'
#' @export
#'
#' @examples
#'
#' # Fitting a random effects model based on c2_dat1
#' \donttest{fit <- lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1)}
#'
#'
#' # Prediction interval using c2_dat2 as future data
#' \donttest{pred_int <- lmer_pi_futmat(model=fit, newdat=c2_dat2, alternative="both", nboot=100)}
#' \donttest{summary(pred_int)}
#'
#' #----------------------------------------------------------------------------
#'
#' # Please note that nboot was set to 100 in order to decrease computing time
#' # of the example. For a valid analysis set nboot=10000.
#'
summary.predint <- function(object, ...){

        # input needs to be a predint object
        if(!inherits(object, "predint")){
                stop("object must be of class predint")
        }

        # get the confidence level
        if(!is.null(attributes(object)$alpha)){
                conf_lev <-  paste((1-attributes(object)$alpha)*100, "%")
        }

        if(is.null(attributes(object)$alpha)){
                conf_lev <-  NULL
        }

        #-----------------------------------------------------------------------
        ### lmer_pi_...

        if(inherits(object, "normalPI")){

                if(object$alternative == "both"){

                        # Title
                        if(object$m > 1){
                                cat("Simultaneous", conf_lev, "prediction interval for", object$m, "future observations \n \n")
                        }

                        if(object$m == 1){
                                cat("Pointwise", conf_lev, "prediction interval for one future observation \n \n")
                        }
                }

                # alternative is not both
                if(object$alternative == "lower"){

                        # Title
                        if(object$m > 1){
                                cat("One-sided simultaneous", conf_lev, "lower prediction limit for", object$m, "future observations \n \n")
                        }

                        if(object$m == 1){
                                cat("One-sided pointwise", conf_lev, "lower prediction limit for one future observation \n \n")
                        }
                }

                # alternative is not both
                if(object$alternative == "upper"){

                        # Title
                        if(object$m > 1){
                                cat("One-sided simultaneous", conf_lev, "upper prediction limit for", object$m, "future observations \n \n")
                        }

                        if(object$m == 1){
                                cat("One-sided pointwise", conf_lev, "upper prediction limit for one future observation \n \n")
                        }
                }

                # Define ql and qu for output
                if(length(object$q) == 2){
                        ql <- rep(object$q[1], times=object$m)
                        qu <- rep(object$q[2], times=object$m)
                        qdf <- data.frame(ql, qu)
                }

                # define q for output
                if(length(object$q) == 1){
                        q <- rep(object$q, times=object$m)
                        qdf <- data.frame(q)
                }

                # output variables as data.frame
                y_star_hat <- data.frame(y_star_hat=object$y_star_hat)
                pred_se <- data.frame(pred_se=object$pred_se)

                # newdat is not available
                if(is.null(object$newdat)){
                        out <- cbind(object$prediction,
                                     y_star_hat,
                                     qdf,
                                     pred_se,
                                     ...)
                }

                # if newdat is given
                if(!is.null(object$newdat)){

                        out <- cbind(object$newdat,
                                     object$prediction,
                                     y_star_hat,
                                     qdf,
                                     pred_se,
                                     ...)

                        if(object$alternative == "both"){
                                out$cover <- object$prediction$lower < object$newdat[,1] & object$newdat[,1] < object$prediction$upper
                        }

                        if(object$alternative == "lower"){
                                out$cover <- object$prediction$lower < object$newdat[,1]
                        }

                        if(object$alternative == "upper"){
                                out$cover <- object$newdat[,1] < object$prediction$upper
                        }
                }

        }


        #-----------------------------------------------------------------------
        ### Overdispersed-binomial PI

        if(inherits(object, "quasiBinomialPI") | inherits(object, "betaBinomialPI")){

                # alternative = both
                if(object$alternative == "both"){

                        # Title
                        if(length(object$newsize)> 1){
                                cat("Simultaneous", conf_lev, "prediction intervals for", length(object$newsize), "future observations \n \n")
                        }

                        if(length(object$newsize) == 1){
                                cat("Pointwise", conf_lev, "prediction interval for one future observation \n \n")
                        }
                }

                # alternative == "upper"
                if(object$alternative == "upper"){

                        # Title
                        if(length(object$newsize) > 1){
                                cat("One-sided simultaneous", conf_lev, "upper prediction limits for", length(object$newsize), "future observations \n \n")
                        }

                        if(length(object$newsize) == 1){
                                cat("One-sided pointwise", conf_lev, "upper prediction limit for one future observation \n \n")
                        }
                }

                # alternative == "upper"
                if(object$alternative == "lower"){

                        # Title
                        if(length(object$newsize) > 1){
                                cat("One-sided simultaneous", conf_lev, "lower prediction limits for", length(object$newsize), "future observations \n \n")
                        }

                        if(length(object$newsize) == 1){
                                cat("One-sided pointwise", conf_lev, "lower prediction limit for one future observation \n \n")
                        }
                }

                # Define ql and qu for output
                if(length(object$q) == 2){
                        ql <- rep(object$q[1], times=length(object$newsize))
                        qu <- rep(object$q[2], times=length(object$newsize))
                        qdf <- data.frame(ql, qu)
                }

                # define q for output
                if(length(object$q) == 1){
                        q <- rep(object$q, times=length(object$newsize))
                        qdf <- data.frame(q)
                }

                # output variables as data.frame
                newsize <- data.frame(newsize=object$newsize)
                y_star_hat <- data.frame(y_star_hat=object$y_star_hat)
                pred_se <- data.frame(pred_se=object$pred_se)

                # newdat is not available
                if(is.null(object$newdat)){
                        out <- cbind(object$prediction,
                                     newsize,
                                     y_star_hat,
                                     qdf,
                                     pred_se,
                                     ...)
                }

                # if newdat is given
                if(!is.null(object$newdat)){

                        out <- cbind(object$newdat,
                                     newsize,
                                     object$prediction,
                                     y_star_hat,
                                     qdf,
                                     pred_se,
                                     ...)

                        if(object$alternative == "both"){
                                out$cover <- out$lower < out[,1] & out[,1] < out$upper
                        }

                        if(object$alternative == "lower"){
                                out$cover <- out$lower < out[,1]
                        }

                        if(object$alternative == "upper"){
                                out$cover <- out[,1] < out$upper
                        }
                }


        }



        #-----------------------------------------------------------------------
        ### Quasi-Poisson PI

        if(inherits(object, "quasiPoissonPI") | inherits(object, "negativeBinomialPI")){

                # alternative = both
                if(object$alternative == "both"){

                        # Title
                        if(length(object$newoffset)> 1){
                                cat("Simultaneous", conf_lev, "prediction intervals for", length(object$newoffset), "future observations \n \n")
                        }

                        if(length(object$newoffset) == 1){
                                cat("Pointwise", conf_lev, "prediction interval for one future observation \n \n")
                        }
                }

                # alternative == "upper"
                if(object$alternative == "upper"){

                        # Title
                        if(length(object$newoffset) > 1){
                                cat("One-sided simultaneous", conf_lev, "upper prediction limits for", length(object$newoffset), "future observations \n \n")
                        }

                        if(length(object$newoffset) == 1){
                                cat("One-sided pointwise", conf_lev, "upper prediction limit for one future observation \n \n")
                        }
                }

                # alternative == "upper"
                if(object$alternative == "lower"){

                        # Title
                        if(length(object$newoffset) > 1){
                                cat("One-sided simultaneous", conf_lev, "lower prediction limits for", length(object$newoffset), "future observations \n \n")
                        }

                        if(length(object$newoffset) == 1){
                                cat("One-sided pointwise", conf_lev, "lower prediction limit for one future observation \n \n")
                        }
                }

                # Define ql and qu for output
                if(length(object$q) == 2){
                        ql <- rep(object$q[1], times=length(object$newoffset))
                        qu <- rep(object$q[2], times=length(object$newoffset))
                        qdf <- data.frame(ql, qu)
                }

                # define q for output
                if(length(object$q) == 1){
                        q <- rep(object$q, times=length(object$newoffset))
                        qdf <- data.frame(q)
                }

                # output variables as data.frame
                newoffset <- data.frame(newoffset=object$newoffset)
                y_star_hat <- data.frame(y_star_hat=object$y_star_hat)
                pred_se <- data.frame(pred_se=object$pred_se)

                # newdat is not available
                if(is.null(object$newdat)){
                        out <- cbind(object$prediction,
                                     newoffset,
                                     y_star_hat,
                                     qdf,
                                     pred_se,
                                     ...)
                }

                # if newdat is given
                if(!is.null(object$newdat)){

                        out <- cbind(object$newdat,
                                     object$prediction,
                                     y_star_hat,
                                     qdf,
                                     pred_se,
                                     ...)

                        if(object$alternative == "both"){
                                out$cover <- out$lower < out[,1] & out[,1] < out$upper
                        }

                        if(object$alternative == "lower"){
                                out$cover <- out$lower < out[,1]
                        }

                        if(object$alternative == "upper"){
                                out$cover <- out[,1] < out$upper
                        }
                }
        }

        #-----------------------------------------------------------------------
        # bootstrap data
        if(inherits(object, "bootstrap")){
                cat(length(object$bs_futdat), "bootstrap samples for both, future and historical observations \n \n")

                no_print <- min(3, length(object$bs_futdat))

                cat("The first", no_print, "bootstrap samples of historical observations \n")
                print(object$bs_histdat[1:no_print])

                cat("The first", no_print, "bootstrap samples of future observations \n")
                print(object$bs_futdat[1:no_print])

                invisible(object)
        }


        #-----------------------------------------------------------------------

        # Output for PIs (and not for bootstrap!)
        if(!inherits(object, "bootstrap")){

                # Print the output
                print(out)

                # Statement if newdat is covered
                if(!is.null(object$newdat)){

                        if(all(out$cover)){
                                cat("\n")
                                cat("All future observations are covered \n")
                        }

                        if(!all(out$cover)){
                                cat("\n")
                                cat("ATTENTION: Not all future observations are covered  \n")
                        }
                }

                # Statement about the algorithm (for calibrated pi)
                if(!is.null(object$algorithm)){

                        if(object$algorithm == "MS22mod" & object$alternative == "both"){
                                cat("\n")
                                cat("Bootstrap calibration was done for each prediction limit separately  \n using a modified  version of Menssen and Schaarschmidt 2022")
                        }

                        else{
                                cat("\n")
                                cat("Bootstrap calibration was done following Menssen and Schaarschmidt 2022")
                        }
                }

                # Statement m<1 is not good for simple pi
                if(is.null(object$algorithm) & nrow(out) > 1){
                        cat("\n")
                        cat("Simple prediction intervals (or limits) are not recommended for m > 1 future observations. \n Please use bootstrap calibration.")
                }


                invisible(out)
        }
}


