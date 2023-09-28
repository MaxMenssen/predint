
#' Print objects of class \code{predint}
#'
#' @param x an object of class \code{predint}
#' @param ... additional arguments passed over to \code{base::cbind()} and \code{base::data.frame()}
#'
#' @return prints output to the console
#' @export
#'
print.predint <- function(x, ...){

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

                # alternative = both
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

                out <- data.frame(x$prediction, ...)
                print(out)
        }

        #-----------------------------------------------------------------------
        ### Beta-binomial PI and Quasi-binomial PI

        if(inherits(x, "betaBinomialPI") | inherits(x, "quasiBinomialPI")){

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

                out <- cbind(x$prediction, data.frame(newsize=x$newsize, ...), ...)
                print(out)

        }

        #-----------------------------------------------------------------------
        ### Quasi-Poisson  or negative-binomial PI

        if(inherits(x, "quasiPoissonPI") | inherits(x, "negativeBinomialPI")){

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


                out <- cbind(x$prediction, data.frame(newoffset=x$newoffset, ...), ...)
                print(out)
        }

        #-----------------------------------------------------------------------
        # bootstrap

        if(inherits(x, "bootstrap")){
                cat(length(x$bs_futdat), "bootstrap samples for both, future and historical observations")
        }
}

