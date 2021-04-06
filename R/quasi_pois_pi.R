#------------------------------------------------------------------------------
#----------------------- Quasi poisson PI -------------------------------------
#------------------------------------------------------------------------------

quasi_pois_pi <- function(histdat, 
                          newdat=NULL, 
                          m=NULL,
                          alternative="both",
                          alpha=0.05, 
                          nboot=2000, 
                          lambda_min=0.01, 
                          lambda_max=10, 
                          traceplot=FALSE, 
                          n_bisec=30){
        
        
        # Relationship between newdat and m
        if(is.null(newdat) & is.null(m)){
                stop("newdat and m are both NULL")
        }
        
        if(!is.null(newdat) & !is.null(m)){
                stop("newdat and m are both defined")
        }
        
        ### historical data
        if(is.data.frame(histdat)==FALSE){
                stop("histdat is not a data.frame")
        }
        
        if(ncol(histdat) != 1){
                stop("histdat has to have one column containing counted observations")
        }
        
        ### Actual data
        if(is.null(newdat) == FALSE){
                if(is.data.frame(newdat)==FALSE){
                        stop("newdat is not a data.frame")
                }
                
                if(ncol(newdat) != 1){
                        stop("newdat has to have one column")
                }
        }
        
        if(is.null(newdat) & is.null(m)==FALSE){
                newdat <- data.frame(x=rep(NA, m))
        }
        
        
        # alternative must be defined 
        if(isTRUE(alternative!="both" && alternative!="lower" && alternative!="upper")){
                stop("alternative must be either both, lower or upper")
        }
        
        #----------------------------------------------------------------------
        #  Number of future clusters
        m <- nrow(newdat)
        
        #-----------------------------------------------------------------------
        ### Historical numbers of observations
        hist_n_total <- nrow(histdat)
        
        #-----------------------------------------------------------------------
        ### Model and parameters
        
        model <- glm(histdat[,1]~1, family=quasipoisson(link="log"))
        
        # Historical phi
        hist_phi <- summary(model)$dispersion
        
        # Store the phi for the final PI
        # newdat$hist_phi <- hist_phi
        
        # If historical phi <= 1, adjust it
        if(hist_phi <= 1){
                
                hist_phi <- 1.001
                # newdat$hist_phi <- hist_phi
                
                warning("historical data is underdispersed (hist_phi <= 1), \n  dispersionparameter was set to 1.001")
        }
        
        
        # Historical lambda
        hist_lambda <- exp(unname(coef(model)))
        newdat$hist_mean <- hist_lambda
        
        #-----------------------------------------------------------------------
        ### Sampling of future data
        
        fut_dat_list <- vector(length=nboot, "list")
        
        for(b in 1:nboot){
                
                # Future observations for calibration
                fut_dat <- data.frame(obs=rqpois(n = m, lambda=hist_lambda,  phi = hist_phi))
                
                # Sampling of the data on which pi and phi is estimated
                bs_pi_se_dat <- data.frame(y=rqpois(n = nrow(histdat), lambda=hist_lambda,  phi = hist_phi))
                
                # bootstrapped parameters
                bs_pi_se_fit <- try(glm(y~1, family=quasipoisson(link="log"), bs_pi_se_dat))
                
                # BS lambda
                bs_lambda <- unname(exp(coef(bs_pi_se_fit)))
                fut_dat$bs_lambda <- bs_lambda
                
                # BS phi
                bs_phi <- max(1, summary(bs_pi_se_fit)$dispersion)
                
                # BS n_total
                bs_n_total <- nrow(bs_pi_se_dat)
                
                # calculation of the prediction se and y_hat
                fut_dat$pred_se <- NA
                
                
                for(d in 1:nrow(fut_dat)){
                        
                        # variance for the future observation
                        bs_fut_var <- bs_phi*bs_lambda*(1 + 1/bs_n_total)
                        
                        fut_dat$pred_se[d] <- sqrt(bs_fut_var)
                }
                
                # calculation of y_hat
                fut_dat$y_hat <- bs_lambda
                
                # Output data
                fut_dat_list[[b]] <- fut_dat
        }
        
        #-----------------------------------------------------------------------
        ### Calculation of the PIs
        
        pi_cover_fun <- function(input, lambda){
                
                input$lower <- NA
                input$upper <- NA
                input$cover <- NA
                
                for(e in 1:nrow(input)){
                        
                        y_hat <- input$y_hat[e]
                        pred_se <- input$pred_se[e]
                        
                        y_fut <- input[,1][e]
                        
                        # Prediction interval 
                        if(alternative=="both"){
                                lower <- y_hat - lambda * pred_se
                                upper <- y_hat + lambda * pred_se
                                
                                input$lower[e] <- lower
                                input$upper[e] <- upper
                                
                                input$cover[e] <- lower < y_fut && y_fut < upper
                        }
                        
                        # Lower prediction bound
                        if(alternative=="lower"){
                                lower <- y_hat - lambda * pred_se
                                
                                input$lower[e] <- lower
                                
                                input$cover[e] <- lower < y_fut 
                                
                        }
                        
                        # Upper prediction bound
                        if(alternative=="upper"){
                                upper <- y_hat + lambda * pred_se
                                
                                input$upper[e] <- upper
                                
                                input$cover[e] <- y_fut < upper
                        }
                        
                }
                
                # Do all intervals cover?
                cover <- all(input$cover)
                
                return(cover)
        }
        
        # Coverage for one lambda based on the BS samples
        cover_fun <- function(lambda){
                
                fut_cover_list <- lapply(X=fut_dat_list, FUN=pi_cover_fun, lambda=lambda)
                
                fut_cover_vec <- as.logical(fut_cover_list)
                
                fut_cover <- sum(fut_cover_vec)/length(fut_cover_vec)
                
                return(fut_cover)
                
        }
        
        bisection <- function(f, quant_min, quant_max, n, tol = 1e-3) {
                
                
                c_i <- vector()
                runval_i <- vector()
                
                # Set tolerable range of cover
                # lower_bound <- 1-(alpha-tol)
                # cover_min <- 1-(alpha+tol)
                
                # if the coverage is smaller for both quant take quant_min
                if ((f(quant_min) > 1-(alpha+tol))) {
                        
                        warning(paste("observed coverage probability for quant_min =", 
                                      f(quant_min), 
                                      "is bigger than 1-alpha+tol =", 
                                      1-alpha+tol))
                        
                        if(traceplot==TRUE){
                                
                                plot(x=quant_min, 
                                     y=f(quant_min)-(1-alpha), 
                                     type="p", 
                                     pch=20, 
                                     xlab="calibration value", 
                                     ylab="obs. coverage - nom. coverage", 
                                     main=paste("f(quant_min) > 1-alpha+tol"),
                                     ylim=c(f(quant_min)-(1-alpha)+tol, -tol))
                                abline(a=0, b=0, lty="dashed")
                                abline(a=tol, b=0, col="grey")
                        }
                        
                        return(quant_min)
                } 
                
                # if the coverage is bigger for both quant take quant_max
                else if ((f(quant_max) < 1-(alpha-tol))) {
                        
                        warning(paste("observed coverage probability for quant_max =", 
                                      f(quant_max), 
                                      "is smaller than 1-alpha-tol =", 
                                      1-alpha-tol))
                        
                        
                        if(traceplot==TRUE){
                                
                                plot(x=quant_max, 
                                     y=f(quant_max)-(1-alpha), 
                                     type="p", pch=20, 
                                     xlab="calibration value",
                                     ylab="obs. coverage - nom. coverage", 
                                     main=paste("f(quant_max) < 1-alpha-tol"),
                                     ylim=c(f(quant_max)-(1-alpha)-tol, tol))
                                abline(a=0, b=0, lty="dashed")
                                abline(a=-tol, b=0, col="grey")
                        }
                        
                        
                        return(quant_max)
                } 
                
                
                else for (i in 1:n) {
                        c <- (quant_min + quant_max) / 2 # Calculate midpoint
                        
                        runval <- (1-alpha)-f(c)
                        
                        # Assigning c and runval into the vectors
                        c_i[i] <- c
                        runval_i[i] <- runval
                        
                        
                        
                        if (abs(runval) < tol) {
                                
                                if(traceplot==TRUE){
                                        
                                        plot(x=c_i,
                                             y=runval_i, 
                                             type="p",
                                             pch=20, 
                                             xlab="calibration value", 
                                             ylab="obs. coverage - nom. coverage", 
                                             main=paste("Trace with", i, "iterations"))
                                        lines(x=c_i, y=runval_i, type="s", col="red")
                                        abline(a=0, b=0, lty="dashed")
                                        abline(a=tol, b=0, col="grey")
                                        abline(a=-tol, b=0, col="grey")
                                }
                                
                                return(c)
                        }
                        
                        # If another iteration is required, 
                        # check the signs of the function at the points c and a and reassign
                        # a or b accordingly as the midpoint to be used in the next iteration.
                        if(sign(runval)==1){
                                quant_min <- c}
                        
                        else if(sign(runval)==-1){
                                quant_max <- c}
                        
                        
                }
                # If the max number of iterations is reached and no root has been found, 
                # return message and end function.
                warning('Too many iterations, but the quantile of the last step are returned')
                
                if(traceplot==TRUE){
                        
                        plot(x=c_i, 
                             y=runval_i,
                             type="p",
                             pch=20, 
                             xlab="calibration value", 
                             ylab="obs. coverage - nom. coverage",  
                             main=paste("Trace with", i, "iterations"))
                        lines(x=c_i, y=runval_i, type="s", col="red")
                        abline(a=0, b=0, lty="dashed")
                        abline(a=tol, b=0, col="grey")
                        abline(a=-tol, b=0, col="grey")
                }
                return(c)
                
        }
        
        # Calculation of the calibrated quantile
        quant_calib <- bisection(f=cover_fun, quant_min=lambda_min, quant_max=lambda_max, n=n_bisec)
        
        # Assign the qalibrated quantile to the output object
        newdat$quant_calib <- quant_calib
        
        for(d in 1:nrow(newdat)){
                
                n_fut <- newdat$total[d]
                
                y_fut <- hist_lambda
                
                fut_var <- hist_phi*hist_lambda*(1 + 1/hist_n_total)
                
                newdat$pred_se[d] <- sqrt(fut_var)
                
                
                # Prediction intervals
                if(alternative=="both"){
                        
                        lower <- y_fut - quant_calib*newdat$pred_se[d]
                        upper <- y_fut + quant_calib*newdat$pred_se[d]
                        
                        newdat$lower[d] <- max(0, lower)
                        newdat$upper[d] <- min(newdat$total[d], upper)
                        
                        if(!is.null(newdat)){
                                newdat$cover[d] <- lower < newdat[d,1] &&  newdat[d,1] < upper 
                        }
                        
                        
                }
                
                # Lower prediction bound
                if(alternative=="lower"){
                        
                        lower <- y_fut - quant_calib*newdat$pred_se[d]
                        
                        newdat$lower[d] <- max(0, lower)
                        
                        if(!is.null(newdat)){
                                newdat$cover[d] <- lower <  newdat[d,1]
                        }
                        
                        
                }
                
                # Upper Prediction bound
                if(alternative=="upper"){
                        upper <- y_fut + quant_calib*newdat$pred_se[d]
                        
                        newdat$upper[d] <- min(newdat$total[d], upper)
                        
                        if(!is.null(newdat)){
                                newdat$cover[d] <- newdat[d,1] < upper 
                        } 
                        
                        
                }
                
        }
        
        out <- newdat[colSums(!is.na(newdat)) > 0]
        
        
        return(out)
        
        
}

