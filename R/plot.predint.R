
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

                # If newdat is not available
                if(is.null(x$newdat)){

                        dat[nrow(dat) + 1 , ] <- NA
                        dat[nrow(dat) , ncol(dat)] <- "predint"
                        dat$obs <- "observations"
                }

                # PI data
                pi_dat <- x$prediction
                pi_dat$y_star_hat <- x$y_star_hat
                pi_dat$data <- "predint"
                pi_dat$obs <-  "observations"


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
                                title <- paste("One-sided simultanious prediction lower limit for", x$m, "future observations")
                        }

                        if(x$m == 1){
                                title <- paste("Onesieded pointwise prediction lower limit for one future observation")
                        }
                }

                # alternative is not both
                if(x$alternative == "upper"){

                        # Title
                        if(x$m > 1){
                                title <- paste("One-sided simultanious prediction upper limit for", x$m, "future observations")
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
        #------------------- Overview about binomial PIs -----------------------
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
                                title <- paste("One-sided simultanious prediction upper limit for", length(x$newsize), "future observations")
                        }

                        if(length(x$newsize) == 1){
                                title <- paste("One-sided pointwise prediction upper limit for one future observation")
                        }
                }

                # alternative == "upper"
                if(x$alternative == "lower"){

                        # Title
                        if(length(x$newsize) > 1){
                                title <- paste("One-sided simultanious prediction lower limit for", length(x$newsize), "future observations")
                        }

                        if(length(x$newsize) == 1){
                                title <- paste("One-sided pointwise prediction lower limit for one future observation")
                        }
                }

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

        #-----------------------------------------------------------------------
        #-------------------- Quasi-Poisson data -------------------------------
        #-----------------------------------------------------------------------

        if(inherits(x, "quasiPoissonPI")){

                dat <- x$histdat
                dat$data <- factor(rep("histdat", times=nrow(dat)))
                offsetf <- factor(dat[,2])
                dat[,2] <-  offsetf
                colnames(dat)[2] <- "offset"

                # If newdat is not available
                if(is.null(x$newdat)){

                        var_names <- colnames(dat)

                        var1 <- rep(NA, times=length(x$newoffset))

                        new_dat <- data.frame(var1,
                                              offset=factor(x$newoffset),
                                              data=rep("predint", length(x$newoffset)))

                        colnames(new_dat) <- var_names
                }

                # if newdat is available
                if(!is.null(x$newdat)){
                        new_dat <- x$newdat
                        new_dat$data <- factor(rep("predint", times=nrow(new_dat)))
                        colnames(new_dat)[2] <- "offset"
                }

                dat <- rbind(dat, new_dat)

                # PI data
                pi_dat <- x$prediction
                pi_dat$offset <- factor(x$newoffset)
                pi_dat$y_star_hat <- x$y_star_hat
                pi_dat$data <- factor(rep("predint", times=nrow(pi_dat)))


                # alternative = both
                if(x$alternative == "both"){

                        # Title
                        if(length(x$newoffset)> 1){
                                title <- paste("Simultanious prediction intervals for", length(x$newoffset), "future observations")
                        }

                        if(length(x$newoffset) == 1){
                                title <- paste("Pointwise prediction interval for one future observation")
                        }
                }

                # alternative == "upper"
                if(x$alternative == "upper"){

                        # Title
                        if(length(x$newoffset) > 1){
                                title <- paste("One-sided simultanious prediction upper limit for", length(x$newoffset), "future observations")
                        }

                        if(length(x$newoffset) == 1){
                                title <- paste("One-sided pointwise prediction upper limit for one future observation")
                        }
                }

                # alternative == "upper"
                if(x$alternative == "lower"){

                        # Title
                        if(length(x$newoffset) > 1){
                                title <- paste("One-sided simultanious prediction lower limit for", length(x$newoffset), "future observations")
                        }

                        if(length(x$newoffset) == 1){
                                title <- paste("One-sided pointwise prediction lower limit for one future observation")
                        }
                }


                # Intervals
                if(x$alternative == "both"){
                        pi_plot <- ggplot(data=dat,
                                          aes(x=offset,
                                              y=dat[,1]))+
                                theme_bw()+
                                facet_grid(~factor(data))+
                                geom_jitter(size=size,
                                            width=width,
                                            height=0,
                                            alpha=alpha)+
                                geom_pointrange(data=pi_dat,
                                                aes(x=offset,
                                                    y=y_star_hat,
                                                    ymin=lower,
                                                    ymax=upper))+
                                ggtitle(title)+
                                xlab("Offset")+
                                ylab("No. of observed objects")+
                                theme(plot.title = element_text(face="bold"),
                                      axis.text.x = element_text(face="bold"),
                                      axis.text.y = element_text(face="bold"))
                }

                # Lower bounds
                if(x$alternative == "lower"){
                        pi_plot <- ggplot(data=dat,
                                          aes(x=offset,
                                              y=dat[,1]))+
                                theme_bw()+
                                facet_grid(~data)+
                                geom_jitter(size=size,
                                            width=width,
                                            height=0,
                                            alpha=alpha)+
                                geom_pointrange(data=pi_dat,
                                                aes(x=offset,
                                                    y=y_star_hat,
                                                    ymin=lower,
                                                    ymax=y_star_hat))+
                                ggtitle(title)+
                                xlab("Offset")+
                                ylab("No. of observed objects")+
                                theme(plot.title = element_text(face="bold"),
                                      axis.text.x = element_text(face="bold"),
                                      axis.text.y = element_text(face="bold"))
                }

                # Upper bounds
                if(x$alternative == "upper"){
                        pi_plot <- ggplot(data=dat,
                                          aes(x=offset,
                                              y=dat[,1]))+
                                theme_bw()+
                                facet_grid(~data)+
                                geom_jitter(size=size,
                                            width=width,
                                            height=0,
                                            alpha=alpha)+
                                geom_pointrange(data=pi_dat,
                                                aes(x=offset,
                                                    y=y_star_hat,
                                                    ymin=y_star_hat,
                                                    ymax=upper))+
                                ggtitle(title)+
                                xlab("Offset")+
                                ylab("No. of observed objects")+
                                theme(plot.title = element_text(face="bold"),
                                      axis.text.x = element_text(face="bold"),
                                      axis.text.y = element_text(face="bold"))
                }
        }


        return(pi_plot)
}











