#-------------------------------------------------------------------------------
#----------------- PI function based on quantile calibration -------------------
#-------------------------------------------------------------------------------

# To do
# - newdat
# - traceplot
# - only models with randiom effects (1|xy)


pi_bs_quant <- function(model, 
                        m, 
                        alpha=0.05,
                        bs_nsim=1000, 
                        traceplot=TRUE, 
                        n_bisec=20){
        
        # Model must be of class lmerMod
        if(class(model) != "lmerMod"){
                stop("class(model) != lmerMod")
        }
        
        # Model must be a random effect model
        if(length(fixef(model)) != 1){
                stop("length(fixef(model)) must be 1 (the model must be a random effects model)")
                
        }
        
        # Extraction the intercept
        mu_hat <- unname(fixef(model))
        
        # SE for the future observation
        se_y_star_hat <- sqrt(sum(c(as.vector(vcov(model)), data.frame(VarCorr(model))$vcov)))
        
        # Number of observations
        n_obs <- nrow(model@frame)
        
        # stop if m > n_obs
        if(m > n_obs){
                stop("m > numbers of original observations")
        }
        
        # stop if m < 1
        if(m > n_obs){
                stop("m < 1")
        }
        
        #----------------------------------------------------------------------
        ### Bootstrapping future observations
        
        # Extracting the observations
        obs_fun <- function(.){
                bs_dat <- .@frame[,1]
        }
        
        # Bootstrap for the observations
        boot_obs <- bootMer(model, obs_fun, nsim = bs_nsim)
        
        # Smallest BS observation
        bs_y_min <- min(t(boot_obs$t))
        
        # Biggest BS observation
        bs_y_max <- max(t(boot_obs$t))
        
        # Bootstrapped data sets
        bsdat_list <- as.list(as.data.frame(t(boot_obs$t)))
        
        # Take only m random observation per data set
        ystar_fun <- function(.){
                y_star <- sample(x=., size=m)
                
                y_star_min <- min(y_star)
                y_star_max <- max(y_star)
                
                c("y_star_min"=y_star_min,
                  "y_star_max"=y_star_max)
                
        }
        
        # List with future observations (y_star)
        ystar_list <- lapply(bsdat_list, ystar_fun)
        
        # print(ystar_list)
        
        #----------------------------------------------------------------------
        ### Bootstrapping the variance of y_star
        
        # Function to get se(y_star)
        se_fun <- function(.){
                bs_var_y_star <- sum(c(as.vector(vcov(.)), data.frame(VarCorr(.))$vcov))
                bs_se_y_star <- sqrt(bs_var_y_star)
                
                bs_mu <- unname(fixef(.))
                
                c(bs_mu=bs_mu, bs_se_y_star=bs_se_y_star)
                
                
                
        }
        
        # Bootstrap
        boot_se <- bootMer(model, se_fun, nsim = bs_nsim)
        
        # Bootstrapped Parameters
        bs_params <- data.frame(boot_se$t)
        
        # print("bs_params")
        # print(bs_params)
        
        # Bootstrapped se
        bs_se<- as.list(as.vector(bs_params$bs_se_y_star))
        
        # Minimum bootstrapped se
        bs_se_min <- min(as.vector(bs_params$bs_se_y_star))
        
        # Maximum bootstrapped se
        bs_se_max <- max(as.vector(bs_params$bs_se_y_star))
        
        # print("c(bs_se_min, bs_se_max)")
        # print(c(bs_se_min, bs_se_max))
        
        # Bootstrapped mu
        bs_mu<- as.list(as.vector(bs_params$bs_mu))
        
        # Minimum bootstrapped mu
        bs_mu_min <- min(as.vector(bs_params$bs_mu))
        
        # Maximum bootstrapped mu
        bs_mu_max <- max(as.vector(bs_params$bs_mu))
        
        
        #----------------------------------------------------------------------
        ### Function for coverage 
        
        coverfun <- function(quant){
                
                pi_list <- vector("list", length=nrow(bs_params))
                
                for(b in 1:nrow(bs_params)){
                        
                        lower <- bs_params$bs_mu[b]-quant*bs_params$bs_se_y_star[b]
                        upper <- bs_params$bs_mu[b]+quant*bs_params$bs_se_y_star[b]
                        
                        pi_list[[b]] <- c("lower"=lower, "upper"=upper, "quant"=quant)
                        
                }
                
                # Check if bith lists have the same length
                if(length(pi_list) != length(ystar_list)){
                        stop("length(pi_list) != length(ystar_list)")
                }
                
                # Length of the lists
                K <- length(pi_list)
                
                # Vector for the Coverage
                cover_vec <- logical(length=K)
                
                for(k in 1:K){
                        cover_vec[k] <- pi_list[[k]]["lower"] < ystar_list[[k]][1] && pi_list[[k]]["upper"] > ystar_list[[k]][2]
                }
                
                # Coverage probabilities as the mean of the 1/0 vector
                coverage <- mean(as.integer(cover_vec))
                
                return(coverage)
                
        }
        
        
        #----------------------------------------------------------------------
        ### Bisection
        
        # Start Values
        quant_max <- min(20, (abs(bs_y_max)+abs(bs_mu_max))/bs_se_min)
        quant_min <- max(1,(abs(bs_y_min)-abs(bs_mu_min))/bs_se_max)
        
        # print("quant_max")
        # print(quant_max)
        # 
        # print("quant_min")
        # print(quant_min)
        
        bisection <- function(f, quant_min, quant_max, n, tol = 1e-3) {
                
                
                c_i <- vector()
                runval_i <- vector()
                
                # Set tolerable range of cover
                cover_max <- 1-(alpha-tol)
                cover_min <- 1-(alpha+tol)
                
                
                # if the coverage is smaller for both quant take quant_min
                if ((f(quant_min) > cover_min)) {
                        
                        print(paste("f(quant_min)", f(quant_min)))
                        return(quant_min)
                } 
                
                # if the coverage is bigger for both quant take quant_max
                else if ((f(quant_max) < cover_max)) {
                        
                        print(paste("(f(quant_max)", f(quant_max)))
                        return(quant_max)
                } 
                
                
                else for (i in 1:n) {
                        c <- (quant_min + quant_max) / 2 # Calculate midpoint
                        
                        # print(paste("i", i))
                        
                        # If the function equals 0 at the midpoint or the midpoint is below the desired tolerance, stop the 
                        # function and return the root.
                        
                        # print(paste("quant_c", c))
                        
                        # print(paste("f(c)=", f(c)))
                        
                        runval <- (1-alpha)-f(c)
                        
                        # print(paste("runval=", runval))
                        
                        # Assigning c1 and runval into the vectors
                        c_i[i] <- c
                        runval_i[i] <- runval
                        
                        
                        
                        if (abs(runval) < tol) {
                                
                                if(traceplot==TRUE){
                                        
                                        # print("runval_i")
                                        # print(runval_i)
                                        
                                        plot(x=c_i, y=runval_i, type="p", pch=20, 
                                             xlab="lambda", ylab="obs. coverage - nom. coverage", 
                                             main=paste("Trace with", i, "iterations"))
                                        lines(x=c_i, y=runval_i, type="s", col="red")
                                        abline(a=0, b=0, lty="dashed")
                                }
                                
                                # print(paste("c",c))
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
                print('Too many iterations, but the quantile of the last step are returned')
                
                if(traceplot==TRUE){
                        
                        # print("runval_i")
                        # print(runval_i)
                        
                        plot(x=c_i, y=runval_i, type="p", pch=20, 
                             # xlab="lambda", ylab=paste("coverage-", nomcov), 
                             main=paste("Trace with", i, "iterations"))
                        lines(x=c_i, y=runval_i, type="s", col="red")
                        abline(a=0, b=0, lty="dashed")
                }
                return(c)
                
        }
        
        # Calculation of the degreees of freedom
        quant_calib <- bisection(f=coverfun, quant_min=quant_min, quant_max=quant_max, n=n_bisec)
        
        # print(paste("df_calib=",df_calib))
        #----------------------------------------------------------------------
        
        # DF calibrated PI
        
        # print("mu_hat")
        # print(mu_hat)
        # 
        # print("quant_calib")
        # print(quant_calib)
        # 
        # print("se_y_star_hat")
        # print(se_y_star_hat)
        
        lower <- mu_hat-quant_calib*se_y_star_hat
        upper <- mu_hat+quant_calib*se_y_star_hat
        
        pi_final <- data.frame("lower"=lower, "upper"=upper, 
                      "quant_calib"=quant_calib, 
                      "m"=m)
        
        
        return(pi_final)
        
}

#------------------------------------------------------------------------------

library(lme4)
library(dplyr)

source("c2_ab_e_drop.R")

# Data Sampling
dat_c2 <- c2_ab_e_drop(mu=-100, n_i=3, n_j=4, n_ij=5,
                       var_i=30, var_j=20, var_ij=10, var_ijk=5, 
                       p_e=0, p_ab=0)

dat_c2_new <- c2_ab_e_drop(mu=-100, n_i=3, n_j=4, n_ij=5,
                           var_i=30, var_j=20, var_ij=10, var_ijk=5, 
                           p_e=0, p_ab=0)
# Fitting the model
lmer_fit_c2 <- lmer(y_ijk ~ 1 + (1|a) + (1|b)  + (1|a:b), dat_c2)

# Run the PI
system.time(pi_calib_c2 <- pi_bs_quant(model=lmer_fit_c2, m=10, traceplot=TRUE))
pi_calib_c2







