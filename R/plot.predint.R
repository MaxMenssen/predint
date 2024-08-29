
#' Plots of \code{predint} objects
#'
#' This function provides methodology for plotting the prediction intervals or
#' limits that are calculated using the functionality of the \pkg{predint} package.
#'
#' @param x object of class \code{predint}
#' @param ... arguments handed over to \code{ggplot2::aes()}
#' @param size size of the dots
#' @param width margin of jittering
#' @param alpha opacity of dot colors
#'
#' @return Since \code{plot.predint()} is based on \code{ggplot2::ggplot}, it returns
#' an object of class \code{c("gg", "ggplot")}.
#'
#' @export
#'
#' @import ggplot2
#'
#' @examples
#' ### PI for quasi-Poisson data
#' pred_int <- quasi_pois_pi(histdat=ames_HCD,
#'                           newoffset=3,
#'                           nboot=100,
#'                           traceplot = FALSE)
#'
#' ### Plot the PI
#' plot(pred_int)
#'
#' ### Since plot.predint is based on ggplot, the grafic can be altered using
#' # the methodology provided via ggplot2
#' plot(pred_int)+
#'      theme_classic()
#'
plot.predint <- function(x,
                         ...,
                         size=4,
                         width=0.05,
                         alpha=0.5){

        # general check if x is of class predint
        if(inherits(x, "predint") == FALSE){
                stop("input object needs to be of class predint")
        }

        # Get the confidence level
        conf_lev <-  (1-attributes(x)$alpha)*100

        #-----------------------------------------------------------------------
        ### No plot for bootstrap objects
        if(inherits(x, "bootstrap")){
                stop("no available plot for objects of class bootstrap")
        }


        #-----------------------------------------------------------------------
        #-------------------- Overview for lmer based PI -----------------------
        #-----------------------------------------------------------------------
        if(inherits(x, "normalPI")){## Simple PI
                if(is.null(x$histdat)){

                        warning("The application of uncalibrated PI is not recommended")

                        # alternative = both
                        if(x$alternative == "both"){

                                # Title
                                if(x$m> 1){
                                        title <- paste("Uncalibrated prediction interval for", x$m, "future observations")
                                }

                                if(x$m == 1){
                                        title <- paste("Uncalibrated prediction interval for one future observation")
                                }
                        }

                        # alternative == "upper"
                        if(x$alternative == "upper"){

                                # Title
                                if(x$m > 1){
                                        title <- paste("Uncalibrated upper prediction limit for", x$m, "future observations")
                                }

                                if(x$m == 1){
                                        title <- paste("Uncalibrated upper prediction limit for one future observation")
                                }
                        }

                        # alternative == "upper"
                        if(x$alternative == "lower"){

                                # Title
                                if(x$m > 1){
                                        title <- paste("Uncalibrated lower prediction limit for", x$m, "future observations")
                                }

                                if(x$m == 1){
                                        title <- paste("Uncalibrated lower prediction limit for one future observation")
                                }
                        }

                        dat <- x$prediction
                        dat$y_star_hat <- x$y_star_hat
                        dat$m <- x$m
                        dat$data <- "predint"
                        # print(dat)

                        if(x$alternative=="both"){
                                pi_plot <- ggplot(data=dat,
                                                  aes(x=factor(.data$m),
                                                      ...))+
                                        theme_bw()+
                                        facet_grid(~factor(.data$data))+
                                        geom_pointrange(aes(y=.data$y_star_hat,
                                                            ymin=.data$lower,
                                                            ymax=.data$upper))+
                                        ggtitle(title)+
                                        xlab("No. of fut. observations")+
                                        ylab("y")+
                                        theme(plot.title = element_text(face="bold"),
                                              axis.text.x = element_text(face="bold"),
                                              axis.text.y = element_text(face="bold"))
                        }

                        if(x$alternative=="lower"){
                                pi_plot <- ggplot(data=dat,
                                                  aes(x=factor(.data$m),
                                                      ...))+
                                        theme_bw()+
                                        facet_grid(~factor(.data$data))+
                                        geom_pointrange(aes(y=.data$y_star_hat,
                                                            ymin=.data$lower,
                                                            ymax=.data$y_star_hat))+
                                        ggtitle(title)+
                                        xlab("No. of fut. observations")+
                                        ylab("y")+
                                        theme(plot.title = element_text(face="bold"),
                                              axis.text.x = element_text(face="bold"),
                                              axis.text.y = element_text(face="bold"))
                        }

                        if(x$alternative=="upper"){
                                pi_plot <- ggplot(data=dat,
                                                  aes(x=factor(.data$m),
                                                      ...))+
                                        theme_bw()+
                                        facet_grid(~factor(.data$data))+
                                        geom_pointrange(aes(y=.data$y_star_hat,
                                                            ymin=.data$y_star_hat,
                                                            ymax=.data$upper))+
                                        ggtitle(title)+
                                        xlab("No. of fut. observations")+
                                        ylab("y")+
                                        theme(plot.title = element_text(face="bold"),
                                              axis.text.x = element_text(face="bold"),
                                              axis.text.y = element_text(face="bold"))
                        }
                }




                else{ # Put the data in the right format
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

                        colnames(dat)[1] <- "y"
                        # PI data
                        pi_dat <- x$prediction
                        pi_dat$y_star_hat <- x$y_star_hat
                        pi_dat$data <- "predint"
                        pi_dat$obs <-  "observations"


                        # # alternative = both
                        if(x$alternative == "both"){

                                # Title
                                if(x$m > 1){
                                        title <- paste("Simultaneous", conf_lev, "% prediction interval for", x$m, "future observations")
                                }

                                if(x$m == 1){
                                        title <- paste("Pointwise", conf_lev, "% prediction interval for one future observation")
                                }
                        }

                        # alternative is not both
                        if(x$alternative == "lower"){

                                # Title
                                if(x$m > 1){
                                        title <- paste("One-sided simultaneous", conf_lev, "% lower prediction limit for", x$m, "future observations")
                                }

                                if(x$m == 1){
                                        title <- paste("One-sided pointwise", conf_lev, "% lower prediction limit for one future observation")
                                }
                        }

                        # alternative is not both
                        if(x$alternative == "upper"){

                                # Title
                                if(x$m > 1){
                                        title <- paste("One-sided simultaneous", conf_lev, "% upper prediction limit for", x$m, "future observations")
                                }

                                if(x$m == 1){
                                        title <- paste("One-sided pointwise", conf_lev, "% upper prediction limit for one future observation")
                                }
                        }

                        ### Grafical overview

                        if(x$alternative == "both"){

                                pi_plot <- ggplot(data=dat,
                                                  aes(x=.data$obs,
                                                      y=.data$y,
                                                      ...))+
                                        theme_bw()+

                                        facet_grid(~.data$data)+
                                        geom_jitter(size=size,
                                                    width=width,
                                                    height=0,
                                                    alpha=alpha)+
                                        geom_pointrange(data=pi_dat,
                                                        aes(y=.data$y_star_hat,
                                                            ymin=.data$lower,
                                                            ymax=.data$upper))+
                                        ggtitle(title)+
                                        ylab(colnames(dat)[1])+
                                        xlab("")+
                                        theme(plot.title = element_text(face="bold"),
                                              axis.text.x = element_text(face="bold"),
                                              axis.text.y = element_text(face="bold"))
                        }

                        if(x$alternative == "lower"){

                                pi_plot <- ggplot(data=dat,
                                                  aes(x=.data$obs,
                                                      y=.data$y,
                                                      ...))+
                                        theme_bw()+

                                        facet_grid(~.data$data)+
                                        geom_jitter(size=size,
                                                    width=width,
                                                    height=0,
                                                    alpha=alpha)+
                                        geom_pointrange(data=pi_dat,
                                                        aes(y=.data$y_star_hat,
                                                            ymin=.data$lower,
                                                            ymax=.data$y_star_hat))+
                                        ggtitle(title)+
                                        ylab(colnames(dat)[1])+
                                        xlab("")+
                                        theme(plot.title = element_text(face="bold"),
                                              axis.text.x = element_text(face="bold"),
                                              axis.text.y = element_text(face="bold"))
                        }

                        if(x$alternative == "upper"){

                                pi_plot <- ggplot(data=dat,
                                                  aes(x=.data$obs,
                                                      y=.data$y,
                                                      ...))+
                                        theme_bw()+

                                        facet_grid(~.data$data)+
                                        geom_jitter(size=size,
                                                    width=width,
                                                    height=0,
                                                    alpha=alpha)+
                                        geom_pointrange(data=pi_dat,
                                                        aes(y=.data$y_star_hat,
                                                            ymin=.data$y_star_hat,
                                                            ymax=.data$upper))+
                                        ggtitle(title)+
                                        ylab(colnames(dat)[1])+
                                        xlab("")+
                                        theme(plot.title = element_text(face="bold"),
                                              axis.text.x = element_text(face="bold"),
                                              axis.text.y = element_text(face="bold"))
                        }
                }

        }



        #-----------------------------------------------------------------------
        #------------------- Overview about binomial PIs -----------------------
        #-----------------------------------------------------------------------

        if(inherits(x, "betaBinomialPI") | inherits(x, "quasiBinomialPI")){

                if(is.null(x$histdat)){

                        warning("The application of uncalibrated PI is not recommended")

                        # alternative = both
                        if(x$alternative == "both"){

                                # Title
                                if(length(x$newsize)> 1){
                                        title <- paste("Uncalibrated prediction intervals for", length(x$newsize), "future observations")
                                }

                                if(length(x$newsize) == 1){
                                        title <- paste("Uncalibrated prediction interval for one future observation")
                                }
                        }

                        # alternative == "upper"
                        if(x$alternative == "upper"){

                                # Title
                                if(length(x$newsize) > 1){
                                        title <- paste("Uncalibrated upper prediction limits for", length(x$newsize), "future observations")
                                }

                                if(length(x$newsize) == 1){
                                        title <- paste("Uncalibrated upper prediction limit for one future observation")
                                }
                        }

                        # alternative == "upper"
                        if(x$alternative == "lower"){

                                # Title
                                if(length(x$newsize) > 1){
                                        title <- paste("Uncalibrated lower prediction limits for", length(x$newsize), "future observations")
                                }

                                if(length(x$newsize) == 1){
                                        title <- paste("Uncalibrated lower prediction limit for one future observation")
                                }
                        }

                        dat <- x$prediction
                        dat$y_star_hat <- x$y_star_hat
                        dat$off_set <- x$newsize
                        dat$data <- "predint"
                        # print(dat)

                        if(x$alternative=="both"){
                                pi_plot <- ggplot(data=dat,
                                                  aes(x=factor(.data$off_set),
                                                      ...))+
                                        theme_bw()+
                                        facet_grid(~factor(.data$data))+
                                        geom_pointrange(aes(y=.data$y_star_hat,
                                                            ymin=.data$lower,
                                                            ymax=.data$upper))+
                                        ggtitle(title)+
                                        xlab("Cluster size")+
                                        ylab("No. of success")+
                                        theme(plot.title = element_text(face="bold"),
                                              axis.text.x = element_text(face="bold"),
                                              axis.text.y = element_text(face="bold"))
                        }

                        if(x$alternative=="lower"){
                                pi_plot <- ggplot(data=dat,
                                                  aes(x=factor(.data$off_set),
                                                      ...))+
                                        theme_bw()+
                                        facet_grid(~factor(.data$data))+
                                        geom_pointrange(aes(y=.data$y_star_hat,
                                                            ymin=.data$lower,
                                                            ymax=.data$y_star_hat))+
                                        ggtitle(title)+
                                        xlab("Cluster size")+
                                        ylab("No. of success")+
                                        theme(plot.title = element_text(face="bold"),
                                              axis.text.x = element_text(face="bold"),
                                              axis.text.y = element_text(face="bold"))
                        }

                        if(x$alternative=="upper"){
                                pi_plot <- ggplot(data=dat,
                                                  aes(x=factor(.data$off_set),
                                                      ...))+
                                        theme_bw()+
                                        facet_grid(~factor(.data$data))+
                                        geom_pointrange(aes(y=.data$y_star_hat,
                                                            ymin=.data$y_star_hat,
                                                            ymax=.data$upper))+
                                        ggtitle(title)+
                                        xlab("Cluster size")+
                                        ylab("No. of success")+
                                        theme(plot.title = element_text(face="bold"),
                                              axis.text.x = element_text(face="bold"),
                                              axis.text.y = element_text(face="bold"))
                        }
                }

                else{# Data management
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
                        dat$size <- factor(dat$size)
                        colnames(dat) <- as.character(c("succ", "fail", "n_clust", "data"))

                        # PI data
                        pi_dat <- x$prediction
                        pi_dat$n_clust <- factor(x$newsize)
                        pi_dat$n_clust_int <- x$newsize
                        pi_dat$y_star_hat <- x$y_star_hat
                        pi_dat$data <- "predint"
                        # colnames(pi_dat) <- as.character(c("lower", "upper", "n_clust", "y_star_hat", "data"))

                        # alternative = both
                        if(x$alternative == "both"){

                                # Title
                                if(length(x$newsize) > 1){
                                        title <- paste("Simultaneous", conf_lev, "% prediction intervals for", length(x$newsize), "future observations")
                                }

                                if(length(x$newsize) == 1){
                                        title <- paste("Pointwise", conf_lev, "% prediction interval for one future observation")
                                }
                        }

                        # alternative == "upper"
                        if(x$alternative == "upper"){

                                # Title
                                if(length(x$newsize) > 1){
                                        title <- paste("One-sided simultaneous", conf_lev, "% upper prediction limits for", length(x$newsize), "future observations")
                                }

                                if(length(x$newsize) == 1){
                                        title <- paste("One-sided pointwise", conf_lev, "% upper prediction limit for one future observation")
                                }
                        }

                        # alternative == "upper"
                        if(x$alternative == "lower"){

                                # Title
                                if(length(x$newsize) > 1){
                                        title <- paste("One-sided simultaneous", conf_lev, "% lower prediction limits for", length(x$newsize), "future observations")
                                }

                                if(length(x$newsize) == 1){
                                        title <- paste("One-sided pointwise", conf_lev, "% lower prediction limit for one future observation")
                                }
                        }

                        # Intervals
                        if(x$alternative == "both"){
                                pi_plot <- ggplot(data=dat,
                                                  aes(x=.data$n_clust,
                                                      y=.data$succ,
                                                      ...))+
                                        theme_bw()+
                                        facet_grid(~.data$data)+
                                        geom_jitter(size=size,
                                                    width=width,
                                                    height=0,
                                                    alpha=alpha)+
                                        geom_pointrange(data=pi_dat,
                                                        aes(y=.data$y_star_hat,
                                                            ymin=.data$lower,
                                                            ymax=.data$upper))+
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
                                                  aes(x=.data$n_clust,
                                                      y=.data$succ,
                                                      ...))+
                                        theme_bw()+
                                        facet_grid(~.data$data)+
                                        geom_jitter(size=size,
                                                    width=width,
                                                    height=0,
                                                    alpha=alpha)+
                                        geom_pointrange(data=pi_dat,
                                                        aes(y=.data$y_star_hat,
                                                            ymin=max(0, .data$lower),
                                                            ymax=.data$n_clust_int))+
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
                                                  aes(x=.data$n_clust,
                                                      y=.data$succ,
                                                      ...))+
                                        theme_bw()+
                                        facet_grid(~.data$data)+
                                        geom_jitter(size=size,
                                                    width=width,
                                                    height=0,
                                                    alpha=alpha)+
                                        geom_pointrange(data=pi_dat,
                                                        aes(y=.data$y_star_hat,
                                                            ymin=0,
                                                            ymax=.data$upper))+
                                        ggtitle(title)+
                                        xlab("Cluster size")+
                                        ylab("No. of success")+
                                        theme(plot.title = element_text(face="bold"),
                                              axis.text.x = element_text(face="bold"),
                                              axis.text.y = element_text(face="bold"))
                        }}

        }

        #-----------------------------------------------------------------------
        #-------------------------- Poisson data -------------------------------
        #-----------------------------------------------------------------------

        if(inherits(x, "quasiPoissonPI") | inherits(x, "negativeBinomialPI")){

                ### Uncalibrated PI

                if(is.null(x$histdat)){

                        warning("The application of uncalibrated PI is not recommended")

                        # alternative = both
                        if(x$alternative == "both"){

                                # Title
                                if(length(x$newoffset)> 1){
                                        title <- paste("Uncalibrated prediction intervals for", length(x$newoffset), "future observations")
                                }

                                if(length(x$newoffset) == 1){
                                        title <- paste("Uncalibrated prediction interval for one future observation")
                                }
                        }

                        # alternative == "upper"
                        if(x$alternative == "upper"){

                                # Title
                                if(length(x$newoffset) > 1){
                                        title <- paste("Uncalibrated upper prediction limits for", length(x$newoffset), "future observations")
                                }

                                if(length(x$newoffset) == 1){
                                        title <- paste("Uncalibrated upper prediction limit for one future observation")
                                }
                        }

                        # alternative == "upper"
                        if(x$alternative == "lower"){

                                # Title
                                if(length(x$newoffset) > 1){
                                        title <- paste("Uncalibrated lower prediction limits for", length(x$newoffset), "future observations")
                                }

                                if(length(x$newoffset) == 1){
                                        title <- paste("Uncalibrated lower prediction limit for one future observation")
                                }
                        }

                        dat <- x$prediction
                        dat$y_star_hat <- x$y_star_hat
                        dat$off_set <- x$newoffset
                        dat$data <- "predint"
                        # print(dat)

                        if(x$alternative=="both"){
                                pi_plot <- ggplot(data=dat,
                                                  aes(x=factor(.data$off_set),
                                                      ...))+
                                        theme_bw()+
                                        facet_grid(~factor(.data$data))+
                                        geom_pointrange(aes(y=.data$y_star_hat,
                                                            ymin=.data$lower,
                                                            ymax=.data$upper))+
                                        ggtitle(title)+
                                        xlab("Offset")+
                                        ylab("No. of observed objects")+
                                        theme(plot.title = element_text(face="bold"),
                                              axis.text.x = element_text(face="bold"),
                                              axis.text.y = element_text(face="bold"))
                        }

                        if(x$alternative=="lower"){
                                pi_plot <- ggplot(data=dat,
                                                  aes(x=factor(.data$off_set),
                                                      ...))+
                                        theme_bw()+
                                        facet_grid(~factor(.data$data))+
                                        geom_pointrange(aes(y=.data$y_star_hat,
                                                            ymin=.data$lower,
                                                            ymax=.data$y_star_hat))+
                                        ggtitle(title)+
                                        xlab("Offset")+
                                        ylab("No. of observed objects")+
                                        theme(plot.title = element_text(face="bold"),
                                              axis.text.x = element_text(face="bold"),
                                              axis.text.y = element_text(face="bold"))
                        }

                        if(x$alternative=="upper"){
                                pi_plot <- ggplot(data=dat,
                                                  aes(x=factor(.data$off_set),
                                                      ...))+
                                        theme_bw()+
                                        facet_grid(~factor(.data$data))+
                                        geom_pointrange(aes(y=.data$y_star_hat,
                                                            ymin=.data$y_star_hat,
                                                            ymax=.data$upper))+
                                        ggtitle(title)+
                                        xlab("Offset")+
                                        ylab("No. of observed objects")+
                                        theme(plot.title = element_text(face="bold"),
                                              axis.text.x = element_text(face="bold"),
                                              axis.text.y = element_text(face="bold"))
                        }

                }

                ### Calibrated PI
                else{
                        # alternative = both
                        if(x$alternative == "both"){

                                # Title
                                if(length(x$newoffset)> 1){
                                        title <- paste("Simultaneous", conf_lev, "% prediction intervals for", length(x$newoffset), "future observations")
                                }

                                if(length(x$newoffset) == 1){
                                        title <- paste("Pointwise", conf_lev, "% prediction interval for one future observation")
                                }
                        }

                        # alternative == "upper"
                        if(x$alternative == "upper"){

                                # Title
                                if(length(x$newoffset) > 1){
                                        title <- paste("One-sided simultaneous", conf_lev, "% upper prediction limits for", length(x$newoffset), "future observations")
                                }

                                if(length(x$newoffset) == 1){
                                        title <- paste("One-sided pointwise", conf_lev, "% upper prediction limit for one future observation")
                                }
                        }

                        # alternative == "upper"
                        if(x$alternative == "lower"){

                                # Title
                                if(length(x$newoffset) > 1){
                                        title <- paste("One-sided simultaneous", conf_lev, "% lower prediction limits for", length(x$newoffset), "future observations")
                                }

                                if(length(x$newoffset) == 1){
                                        title <- paste("One-sided pointwise", conf_lev, "% lower prediction limit for one future observation")
                                }
                        }

                        dat <- x$histdat
                        dat$data <- factor(rep("histdat", times=nrow(dat)))
                        offsetf <- factor(dat[,2])
                        dat[,2] <-  offsetf
                        colnames(dat)[1] <- "y"
                        colnames(dat)[2] <- "off_set"

                        # If newdat is not available
                        if(is.null(x$newdat)){

                                var_names <- colnames(dat)

                                var1 <- rep(NA, times=length(x$newoffset))

                                new_dat <- data.frame(var1,
                                                      off_set=factor(x$newoffset),
                                                      data=rep("predint", length(x$newoffset)))

                                colnames(new_dat) <- var_names
                        }

                        # if newdat is available
                        if(!is.null(x$newdat)){
                                new_dat <- x$newdat
                                new_dat$data <- factor(rep("predint", times=nrow(new_dat)))
                                colnames(new_dat)[1] <- "y"
                                colnames(new_dat)[2] <- "off_set"
                        }

                        dat <- rbind(dat, new_dat)

                        # PI data
                        pi_dat <- x$prediction
                        pi_dat$off_set <- factor(x$newoffset)
                        pi_dat$y_star_hat <- x$y_star_hat
                        pi_dat$data <- factor(rep("predint", times=nrow(pi_dat)))

                        # print(dat)
                        # print(str(dat))
                        # print(colnames(dat))

                        # Intervals
                        if(x$alternative == "both"){
                                pi_plot <- ggplot(data=dat,
                                                  aes(x=.data$off_set,
                                                      y=.data$y,
                                                      ...))+
                                        theme_bw()+
                                        facet_grid(~factor(.data$data))+
                                        geom_jitter(size=size,
                                                    width=width,
                                                    height=0,
                                                    alpha=alpha)+
                                        geom_pointrange(data=pi_dat,
                                                        aes(x=.data$off_set,
                                                            y=.data$y_star_hat,
                                                            ymin=.data$lower,
                                                            ymax=.data$upper))+
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
                                                  aes(x=.data$off_set,
                                                      y=.data$y,
                                                      ...))+
                                        theme_bw()+
                                        facet_grid(~.data$data)+
                                        geom_jitter(size=size,
                                                    width=width,
                                                    height=0,
                                                    alpha=alpha)+
                                        geom_pointrange(data=pi_dat,
                                                        aes(x=.data$off_set,
                                                            y=.data$y_star_hat,
                                                            ymin=.data$lower,
                                                            ymax=.data$y_star_hat))+
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
                                                  aes(x=.data$off_set,
                                                      y=.data$y,
                                                      ...))+
                                        theme_bw()+
                                        facet_grid(~.data$data)+
                                        geom_jitter(size=size,
                                                    width=width,
                                                    height=0,
                                                    alpha=alpha)+
                                        geom_pointrange(data=pi_dat,
                                                        aes(x=.data$off_set,
                                                            y=.data$y_star_hat,
                                                            ymin=.data$y_star_hat,
                                                            ymax=.data$upper))+
                                        ggtitle(title)+
                                        xlab("Offset")+
                                        ylab("No. of observed objects")+
                                        theme(plot.title = element_text(face="bold"),
                                              axis.text.x = element_text(face="bold"),
                                              axis.text.y = element_text(face="bold"))
                        }
                }

        }


        return(pi_plot)
}





