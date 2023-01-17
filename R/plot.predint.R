
#' Plots of prediction intervals
#'
#' @param x
#' @param size
#' @param width
#' @param alpha
#' @param scale
#'
#' @return
#' @export
#'
#' @import ggplot2
#'
#' @examples
plot.predint <- function(x,
                         size=4,
                         width=0.05,
                         alpha=0.5){

        # general check if x is of class predint
        if(inherits(x, "predint") == FALSE){
                stop("input object needs to be of class predint")
        }

        #-----------------------------------------------------------------------
        #-------------------- Overview for lmer based PI -----------------------
        #-----------------------------------------------------------------------
        if(inherits(x, "normalPI")){

                # Put the data in the right format
                dat <- x$histdat
                dat$data <- "histdat"

                # Add newdat if available
                if(!is.null(x$newdat)){
                        newdat <- x$newdat
                        newdat$data <- "predint"

                        dat <- rbind(dat, newdat)
                        dat$obs <- "observations"
                }

                # If newdat is not available (does not work)
                # if(is.null(x$newdat)){
                #         newdat <- as.data.frame(matrix(c(rep(NA,
                #                                              times=(ncol(x$histdat))),
                #                                          "predint"),
                #                                        nrow=1))
                #
                #         colnames(newdat) <- colnames(dat)
                #         dat <- rbind(dat, newdat)
                #         dat$obs <- "observations"
                # }

                # PI data
                pi_dat <- x$prediction
                pi_dat$y_star_hat <- x$y_star_hat
                pi_dat$data <- "predint"
                pi_dat$obs <-  "observations"

                print(dat)
                print(str(dat))

                # # alternative = both
                if(x$alternative == "both"){

                        # Title
                        if(x$m > 1){
                                title <- paste("Simultanious prediction interval for", x$m, "future observations")
                        }

                        if(x$m == 1){
                                title <- paste("Pointwise prediction interval for one future observation")
                        }
                }

                # alternative is not both
                if(x$alternative == "lower"){

                        # Title
                        if(x$m > 1){
                                title <- paste("Onesided simultanious prediction lower limit for", x$m, "future observations")
                        }

                        if(x$m == 1){
                                title <- paste("Onesieded pointwise prediction lower limit for one future observation")
                        }
                }

                # alternative is not both
                if(x$alternative == "upper"){

                        # Title
                        if(x$m > 1){
                                title <- paste("Onesided simultanious prediction upper limit for", x$m, "future observations")
                        }

                        if(x$m == 1){
                                title <- paste("Onesieded pointwise prediction upper limit for one future observation")
                        }
                }

                ### Grafical overview

                if(x$alternative == "both"){

                        pi_plot <- ggplot(data=dat,
                                          aes(x=obs,
                                              y=dat[,1]))+
                                theme_bw()+

                                facet_grid(~data)+
                                geom_jitter(size=size,
                                            width=width,
                                            height=0,
                                            alpha=alpha)+
                                geom_pointrange(data=pi_dat,
                                                aes(y=y_star_hat,
                                                    ymin=lower,
                                                    ymax=upper))+
                                ggtitle(title)+
                                ylab(colnames(dat)[1])+
                                xlab("")+
                                theme(plot.title = element_text(face="bold"),
                                      axis.text.x = element_text(face="bold"),
                                      axis.text.y = element_text(face="bold"))
                }

                if(x$alternative == "lower"){

                        pi_plot <- ggplot(data=dat,
                                          aes(x=obs,
                                              y=dat[,1]))+
                                theme_bw()+

                                facet_grid(~data)+
                                geom_jitter(size=size,
                                            width=width,
                                            height=0,
                                            alpha=alpha)+
                                geom_pointrange(data=pi_dat,
                                                aes(y=y_star_hat,
                                                    ymin=lower,
                                                    ymax=y_star_hat))+
                                ggtitle(title)+
                                ylab(colnames(dat)[1])+
                                xlab("")+
                                theme(plot.title = element_text(face="bold"),
                                      axis.text.x = element_text(face="bold"),
                                      axis.text.y = element_text(face="bold"))
                }

                if(x$alternative == "upper"){

                        pi_plot <- ggplot(data=dat,
                                          aes(x=obs,
                                              y=dat[,1]))+
                                theme_bw()+

                                facet_grid(~data)+
                                geom_jitter(size=size,
                                            width=width,
                                            height=0,
                                            alpha=alpha)+
                                geom_pointrange(data=pi_dat,
                                                aes(y=y_star_hat,
                                                    ymin=y_star_hat,
                                                    ymax=upper))+
                                ggtitle(title)+
                                ylab(colnames(dat)[1])+
                                xlab("")+
                                theme(plot.title = element_text(face="bold"),
                                      axis.text.x = element_text(face="bold"),
                                      axis.text.y = element_text(face="bold"))
                }

        }

        #-----------------------------------------------------------------------
        #------------------- Overview about beta bin. PI -----------------------
        #-----------------------------------------------------------------------

        if(inherits(x, "betaBinomialPI") | inherits(x, "quasiBinomialPI")){

                # Data management
                dat <- x$histdat
                dat$size <- x$histsize
                dat$data <- "histdat"

                # If newdat is not available
                if(is.null(x$newdat)){

                        var_names <- colnames(dat)

                        var1 <- rep(NA, times=length(x$newsize))
                        var2 <- rep(NA, times=length(x$newsize))

                        new_dat <- data.frame(var1,
                                              var2,
                                              size=x$newsize,
                                              data=rep("predint", length(x$newsize)))

                        colnames(new_dat) <- var_names
                }

                # if newdat is available
                if(!is.null(x$newdat)){
                        new_dat <- x$newdat
                        new_dat$size <- x$newsize
                        new_dat$data <- "predint"
                }

                dat <- rbind(dat, new_dat)

                # PI data
                pi_dat <- x$prediction
                pi_dat$size <- x$newsize
                pi_dat$y_star_hat <- x$y_star_hat
                pi_dat$data <- "predint"

                # alternative = both
                if(x$alternative == "both"){

                        # Title
                        if(length(x$newsize)> 1){
                                title <- paste("Simultanious prediction intervals for", length(x$newsize), "future observations")
                        }

                        if(length(x$newsize) == 1){
                                title <- paste("Pointwise prediction interval for one future observation")
                        }
                }

                # alternative == "upper"
                if(x$alternative == "upper"){

                        # Title
                        if(length(x$newsize) > 1){
                                title <- paste("Onesided simultanious prediction upper limit for", length(x$newsize), "future observations")
                        }

                        if(length(x$newsize) == 1){
                                title <- paste("Onesided pointwise prediction upper limit for one future observation")
                        }
                }

                # alternative == "upper"
                if(x$alternative == "lower"){

                        # Title
                        if(length(x$newsize) > 1){
                                title <- paste("Onesided simultanious prediction lower limit for", length(x$newsize), "future observations")
                        }

                        if(length(x$newsize) == 1){
                                title <- paste("Onesided pointwise prediction lower limit for one future observation")
                        }
                }

                ### Grafic on response scale


                        # Intervals
                        if(x$alternative == "both"){
                                pi_plot <- ggplot(data=dat,
                                                  aes(x=factor(size),
                                                      y=dat[,1]))+
                                        theme_bw()+
                                        facet_grid(~data)+
                                        geom_jitter(size=size,
                                                    width=width,
                                                    height=0,
                                                    alpha=alpha)+
                                        geom_pointrange(data=pi_dat,
                                                        aes(y=y_star_hat,
                                                            ymin=lower,
                                                            ymax=upper))+
                                        ggtitle(title)+
                                        xlab("Cluster size")+
                                        ylab("No. of success")+
                                        theme(plot.title = element_text(face="bold"),
                                              axis.text.x = element_text(face="bold"),
                                              axis.text.y = element_text(face="bold"))
                        }

                        # Lower bounds
                        if(x$alternative == "lower"){
                                pi_plot <- ggplot(data=dat,
                                                  aes(x=factor(size),
                                                      y=dat[,1]))+
                                        theme_bw()+
                                        facet_grid(~data)+
                                        geom_jitter(size=size,
                                                    width=width,
                                                    height=0,
                                                    alpha=alpha)+
                                        geom_pointrange(data=pi_dat,
                                                        aes(y=y_star_hat,
                                                            ymin= lower,
                                                            ymax=size))+
                                        ggtitle(title)+
                                        xlab("Cluster size")+
                                        ylab("No. of success")+
                                        theme(plot.title = element_text(face="bold"),
                                              axis.text.x = element_text(face="bold"),
                                              axis.text.y = element_text(face="bold"))
                        }

                        # Upper bounds
                        if(x$alternative == "upper"){
                                pi_plot <- ggplot(data=dat,
                                                  aes(x=factor(size),
                                                      y=dat[,1]))+
                                        theme_bw()+
                                        facet_grid(~data)+
                                        geom_jitter(size=size,
                                                    width=width,
                                                    height=0,
                                                    alpha=alpha)+
                                        geom_pointrange(data=pi_dat,
                                                        aes(y=y_star_hat,
                                                            ymin=0,
                                                            ymax=upper))+
                                        ggtitle(title)+
                                        xlab("Cluster size")+
                                        ylab("No. of success")+
                                        theme(plot.title = element_text(face="bold"),
                                              axis.text.x = element_text(face="bold"),
                                              axis.text.y = element_text(face="bold"))
                        }
        }

        return(pi_plot)
}






# pred_int <- quasi_bin_pi(histdat=bb_dat1,
#                         newsize=c(40, 50, 60),
#                         nboot=1000,
#                         traceplot = FALSE,
#                         alternative="upper")

# pred_int <- quasi_bin_pi(histdat=bb_dat1,
#                         newdat=bb_dat2,
#                         nboot=1000,
#                         traceplot=FALSE,
#                         alternative="lower")
#
#
# fit <- lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1)
#
# pred_int <- lmer_pi_futmat(model=fit,
#                            newdat=c2_dat2,
#                            alternative="lower",
#                            nboot=1000,
#                            traceplot=FALSE)
#
# # Das geht noch nicht!!
# pred_int <- lmer_pi_futmat(model=fit,
#                            newdat=1,
#                            alternative="lower",
#                            nboot=1000,
#                            traceplot=FALSE)
#
#
# plot(pred_int)
#
# #
# #
# as.data.frame(matrix(c(rep(NA, times=(ncol(pred_int$histdat))),"predint"),
#        nrow=1))


