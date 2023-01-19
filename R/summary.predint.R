



#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
summary.predint <- function(x){

        # input needs to be a predint object
        if(!inherits(x, "predint")){
                stop("x must be of class predint")
        }

        # get the confidence level
        conf_lev <-  (1-attributes(x)$alpha)*100

        #-----------------------------------------------------------------------
        ### lmer_pi_...

        if(inherits(x, "normalPI")){

        }

        #-----------------------------------------------------------------------
        ### Beta-binomial PI

        if(inherits(x, "betaBinomialPI")){

        }

        #-----------------------------------------------------------------------
        ### Quasi-binomial PI

        if(inherits(x, "quasiBinomialPI")){

        }


        #-----------------------------------------------------------------------
        ### Quasi-Poisson PI

        if(inherits(x, "quasiPoissonPI")){

                # alternative = both
                if(x$alternative == "both"){

                        # Title
                        if(length(x$newoffset)> 1){
                                cat("Simultanious", conf_lev, "% prediction intervals for", length(x$newoffset), "future observations \n \n")
                        }

                        if(length(x$newoffset) == 1){
                                cat("Pointwise", conf_lev, "% prediction interval for one future observation \n \n")
                        }
                }

                # alternative == "upper"
                if(x$alternative == "upper"){

                        # Title
                        if(length(x$newoffset) > 1){
                                cat("One-sided simultanious", conf_lev, "% upper prediction limits for", length(x$newoffset), "future observations \n \n")
                        }

                        if(length(x$newoffset) == 1){
                                cat("One-sided pointwise", conf_lev, "% upper prediction limit for one future observation \n \n")
                        }
                }

                # alternative == "upper"
                if(x$alternative == "lower"){

                        # Title
                        if(length(x$newoffset) > 1){
                                cat("One-sided simultanious", conf_lev, "% lower prediction limits for", length(x$newoffset), "future observations \n \n")
                        }

                        if(length(x$newoffset) == 1){
                                cat("One-sided pointwise", conf_lev, "% lower prediction limit for one future observation \n \n")
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

        print(out)

        if(all(out$cover)){
                cat("\n All future observations are covered")
        }

        if(!all(out$cover)){
                cat("\n ATTENTION: Not all future observations are covered")
        }

        invisible(out)


}



# pred_int <- quasi_pois_pi(histdat=data.frame(qp_dat1),
#                           newdat =qp_dat1,
#                           nboot=1000,
#                           traceplot = FALSE,
#                           algorithm = "MS21mod",
#                           alternative = "upper")
#
# sdf <- summary(pred_int)
# sdf


# attributes(pred_int)
#
#
# summary(pred_int)
#
# cbind(pred_int$prediction,
#       pred_int$newoffset,
#       pred_int$y_star_hat,
#       pred_int$q,
#       pred_int$pred_se)
#
#
# fit <- lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1)
# summary(fit)
#
# ?lmer
