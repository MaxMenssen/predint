



#' Summarizing objects of class \code{predint}
#'
#' This function gives a summary about the prediction intervals (and limits)
#' computed with \pkg{predint}.
#'
#' @param x object of class \code{predint}
#'
#' @return A \code{data.frame} containing the actual data (if provided via \code{newdat}),
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
summary.predint <- function(x){

        # input needs to be a predint object
        if(!inherits(x, "predint")){
                stop("x must be of class predint")
        }

        # get the confidence level
        if(!is.null(attributes(x)$alpha)){
                conf_lev <-  paste((1-attributes(x)$alpha)*100, "%")
        }

        if(is.null(attributes(x)$alpha)){
                conf_lev <-  NULL
        }

        #-----------------------------------------------------------------------
        ### lmer_pi_...

        if(inherits(x, "normalPI")){

                if(x$alternative == "both"){

                        # Title
                        if(x$m > 1){
                                cat("Simultanious", conf_lev, "prediction interval for", x$m, "future observations \n \n")
                        }

                        if(x$m == 1){
                                cat("Pointwise", conf_lev, "prediction interval for one future observation \n \n")
                        }
                }

                # alternative is not both
                if(x$alternative == "lower"){

                        # Title
                        if(x$m > 1){
                                cat("One-sided simultanious", conf_lev, "lower prediction limit for", x$m, "future observations \n \n")
                        }

                        if(x$m == 1){
                                cat("One-sided pointwise", conf_lev, "lower prediction limit for one future observation \n \n")
                        }
                }

                # alternative is not both
                if(x$alternative == "upper"){

                        # Title
                        if(x$m > 1){
                                cat("One-sided simultanious", conf_lev, "upper prediction limit for", x$m, "future observations \n \n")
                        }

                        if(x$m == 1){
                                cat("One-sided pointwise", conf_lev, "upper prediction limit for one future observation \n \n")
                        }
                }

                # Define ql and qu for output
                if(length(x$q) == 2){
                        ql <- rep(x$q[1], times=x$m)
                        qu <- rep(x$q[2], times=x$m)
                        qdf <- data.frame(ql, qu)
                }

                # define q for output
                if(length(x$q) == 1){
                        q <- rep(x$q, times=x$m)
                        qdf <- data.frame(q)
                }

                # output variables as data.frame
                y_star_hat <- data.frame(y_star_hat=x$y_star_hat)
                pred_se <- data.frame(pred_se=x$pred_se)

                # newdat is not available
                if(is.null(x$newdat)){
                        out <- cbind(x$prediction,
                                     y_star_hat,
                                     qdf,
                                     pred_se)
                }

                # if newdat is given
                if(!is.null(x$newdat)){

                        out <- cbind(x$newdat,
                                     x$prediction,
                                     y_star_hat,
                                     qdf,
                                     pred_se)

                        if(x$alternative == "both"){
                                out$cover <- out$lower < out[,1] & out[,1] < out$upper
                        }

                        if(x$alternative == "lower"){
                                out$cover <- out$lower < out[,1]
                        }

                        if(x$alternative == "upper"){
                                out$cover <- out[,1] < out$upper
                        }
                }

        }


        #-----------------------------------------------------------------------
        ### Overdispersed-binomial PI

        if(inherits(x, "quasiBinomialPI") | inherits(x, "betaBinomialPI")){

                # alternative = both
                if(x$alternative == "both"){

                        # Title
                        if(length(x$newsize)> 1){
                                cat("Simultanious", conf_lev, "prediction intervals for", length(x$newsize), "future observations \n \n")
                        }

                        if(length(x$newsize) == 1){
                                cat("Pointwise", conf_lev, "prediction interval for one future observation \n \n")
                        }
                }

                # alternative == "upper"
                if(x$alternative == "upper"){

                        # Title
                        if(length(x$newsize) > 1){
                                cat("One-sided simultanious", conf_lev, "upper prediction limits for", length(x$newsize), "future observations \n \n")
                        }

                        if(length(x$newsize) == 1){
                                cat("One-sided pointwise", conf_lev, "upper prediction limit for one future observation \n \n")
                        }
                }

                # alternative == "upper"
                if(x$alternative == "lower"){

                        # Title
                        if(length(x$newsize) > 1){
                                cat("One-sided simultanious", conf_lev, "lower prediction limits for", length(x$newsize), "future observations \n \n")
                        }

                        if(length(x$newsize) == 1){
                                cat("One-sided pointwise", conf_lev, "lower prediction limit for one future observation \n \n")
                        }
                }

                # Define ql and qu for output
                if(length(x$q) == 2){
                        ql <- rep(x$q[1], times=length(x$newsize))
                        qu <- rep(x$q[2], times=length(x$newsize))
                        qdf <- data.frame(ql, qu)
                }

                # define q for output
                if(length(x$q) == 1){
                        q <- rep(x$q, times=length(x$newsize))
                        qdf <- data.frame(q)
                }

                # output variables as data.frame
                newsize <- data.frame(newsize=x$newsize)
                y_star_hat <- data.frame(y_star_hat=x$y_star_hat)
                pred_se <- data.frame(pred_se=x$pred_se)

                # newdat is not available
                if(is.null(x$newdat)){
                        out <- cbind(x$prediction,
                                     newsize,
                                     y_star_hat,
                                     qdf,
                                     pred_se)
                }

                # if newdat is given
                if(!is.null(x$newdat)){

                        out <- cbind(x$newdat,
                                     newsize,
                                     x$prediction,
                                     y_star_hat,
                                     qdf,
                                     pred_se)

                        if(x$alternative == "both"){
                                out$cover <- out$lower < out[,1] & out[,1] < out$upper
                        }

                        if(x$alternative == "lower"){
                                out$cover <- out$lower < out[,1]
                        }

                        if(x$alternative == "upper"){
                                out$cover <- out[,1] < out$upper
                        }
                }


        }



        #-----------------------------------------------------------------------
        ### Quasi-Poisson PI

        if(inherits(x, "quasiPoissonPI")){

                # alternative = both
                if(x$alternative == "both"){

                        # Title
                        if(length(x$newoffset)> 1){
                                cat("Simultanious", conf_lev, "prediction intervals for", length(x$newoffset), "future observations \n \n")
                        }

                        if(length(x$newoffset) == 1){
                                cat("Pointwise", conf_lev, "prediction interval for one future observation \n \n")
                        }
                }

                # alternative == "upper"
                if(x$alternative == "upper"){

                        # Title
                        if(length(x$newoffset) > 1){
                                cat("One-sided simultanious", conf_lev, "upper prediction limits for", length(x$newoffset), "future observations \n \n")
                        }

                        if(length(x$newoffset) == 1){
                                cat("One-sided pointwise", conf_lev, "upper prediction limit for one future observation \n \n")
                        }
                }

                # alternative == "upper"
                if(x$alternative == "lower"){

                        # Title
                        if(length(x$newoffset) > 1){
                                cat("One-sided simultanious", conf_lev, "lower prediction limits for", length(x$newoffset), "future observations \n \n")
                        }

                        if(length(x$newoffset) == 1){
                                cat("One-sided pointwise", conf_lev, "lower prediction limit for one future observation \n \n")
                        }
                }

                # Define ql and qu for output
                if(length(x$q) == 2){
                        ql <- rep(x$q[1], times=length(x$newoffset))
                        qu <- rep(x$q[2], times=length(x$newoffset))
                        qdf <- data.frame(ql, qu)
                }

                # define q for output
                if(length(x$q) == 1){
                        q <- rep(x$q, times=length(x$newoffset))
                        qdf <- data.frame(q)
                }

                # output variables as data.frame
                newoffset <- data.frame(newoffset=x$newoffset)
                y_star_hat <- data.frame(y_star_hat=x$y_star_hat)
                pred_se <- data.frame(pred_se=x$pred_se)

                # newdat is not available
                if(is.null(x$newdat)){
                        out <- cbind(x$prediction,
                                     newoffset,
                                     y_star_hat,
                                     qdf,
                                     pred_se)
                }

                # if newdat is given
                if(!is.null(x$newdat)){

                        out <- cbind(x$newdat,
                                     x$prediction,
                                     y_star_hat,
                                     qdf,
                                     pred_se)

                        if(x$alternative == "both"){
                                out$cover <- out$lower < out[,1] & out[,1] < out$upper
                        }

                        if(x$alternative == "lower"){
                                out$cover <- out$lower < out[,1]
                        }

                        if(x$alternative == "upper"){
                                out$cover <- out[,1] < out$upper
                        }
                }
        }

        #-----------------------------------------------------------------------

        # Print the output
        print(out)

        # Statement if newdat is covered
        if(!is.null(x$newdat)){

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
        if(!is.null(x$algorithm)){

                if(x$algorithm == "MS22mod" & x$alternative == "both"){
                        cat("\n")
                        cat("Bootstrap calibration was done for each prediction limit seperately \n using a modiefied version of Menssen and Schaarschmidt 2022")
                }

                else{
                        cat("\n")
                        cat("Bootstrap calibration was done following Menssen and Schaarschmidt 2022")
                }
        }

        # Statement m<1 is not good for simple pi
        if(is.null(x$algorithm) & nrow(out) > 1){
                cat("\n")
                cat("Simple prediction intervals (or limits) are not recommended for m > 1 future observations. \n Please use bootstrap calibration.")
        }


        # Output
        invisible(out)
}



