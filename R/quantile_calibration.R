#-------------------------------------------------------------------------------
#----------------- PI function based on quantile calibration -------------------
#-------------------------------------------------------------------------------

# To do
# - newdat
# - alternative
# - traceplot DONE
# - only models with randiom effects (1|xy) DONE

# histdat,
# newdat=NULL,
# m=NULL,
# alternative="both",
# alpha=0.05,
# nboot=2000,
# lambda_min=0.01,
# lambda_max=10,
# traceplot=FALSE,
# n_bisec=30

lmer_pi <- function(model,
                    newdat=NULL,
                    m=NULL,
                    # alternative="both",
                    alpha=0.05,
                    nboot=1000,
                    lambda_min=0.01,
                    lambda_max=10,
                    traceplot=TRUE,
                    n_bisec=30){

        # Model must be of class lmerMod
        if(class(model) != "lmerMod"){
                stop("class(model) != lmerMod")
        }

        # Model must be a random effect model
        if(length(fixef(model)) != 1){
                stop("length(fixef(model)) must be 1 (the model must be a random effects model)")

        }


        ### All random effects must be specified as (1|random_effect)
        # Get forumla object
        f <- lmer_fit_c2@call$formula

        # Right part of formula as a string
        fs <- as.character(f)[3]

        # First substitue all whitespace characters with nothing ("") to make shure they don't disturb.
        # '\\s' is regex for 'all whitespace characters like space, tab, line break, ...)'
        fs <- gsub("\\s", "", fs)

        # Are there any occurances where '|' is not preceded by a '1'?
        # '[^1]' is regex for 'not 1' and '\\|' is just regex for '|'.
        wrong_formula <- grepl('[^1]\\|', fs)

        if(wrong_formula){
                stop("Random effects must be specified as (1|random_effect)")
        }



        # Relationship between newdat and m
        if(is.null(newdat) & is.null(m)){
                stop("newdat and m are both NULL")
        }

        if(!is.null(newdat) & !is.null(m)){
                stop("newdat and m are both defined")
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
        # if(isTRUE(alternative!="both" && alternative!="lower" && alternative!="upper")){
        #         stop("alternative must be either both, lower or upper")
        # }

        #----------------------------------------------------------------------


        # Extraction of the intercept
        mu_hat <- unname(fixef(model))

        # SE for the future observation
        se_y_star_hat <- sqrt(sum(c(as.vector(vcov(model)),
                                    data.frame(VarCorr(model))$vcov)))

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
        boot_obs <- bootMer(model, obs_fun, nsim = nboot)

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
        boot_se <- bootMer(model, se_fun, nsim = nboot)

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

        bisection <- function(f, quant_min, quant_max, n, tol = 1e-3) {


                c_i <- vector()
                runval_i <- vector()

                # Set tolerable range of cover
                # cover_max <- 1-(alpha-tol)
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

        # Calculation of the degreees of freedom
        quant_calib <- bisection(f=coverfun, quant_min=lambda_min, quant_max=lambda_max, n=n_bisec)

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

        pi_final <- data.frame("m"=m,
                               "histmean"=mu_hat,
                               "quant_calib"=quant_calib,
                               "pred_se"=se_y_star_hat,
                               "lower"=lower,
                               "upper"=upper)


        return(pi_final)

}

#------------------------------------------------------------------------------

library(lme4)
library(dplyr)

# Function for data sampling
c2_ab_e_drop <- function(mu, n_i, n_j, n_ij, var_i, var_j, var_ij, var_ijk, p_e, p_ab){

        # Vector for the intercept
        mu <- rep(x=mu, times=(n_i * n_j * n_ij))

        # vector for factor a
        a_i <- rep(x=rnorm(n=n_i, sd=var_i), each=n_ij, times=n_j)

        # vector for factor b
        b_j <- rep(x=rnorm(n=n_j, sd=var_j), each=n_i*n_ij)

        # vector for the interaction
        ab_ij <- rep(x=rnorm(n=n_j*n_i, sd=var_ij), each=n_ij)

        # vector of the residuals
        e_ijk <- rnorm(n=(n_i*n_j*n_ij), sd=var_ijk)

        # vector of the observations
        y_ijk <- mu + a_i + b_j + ab_ij + e_ijk

        # Sampling index vector for  NAs
        na_index1 <- rbinom(n=length(y_ijk), size=1, prob=p_e)

        # Setting some observations NA
        for(i in 1:length(y_ijk)){

                if(na_index1[i]==1){
                        y_ijk[i] <- NA
                }
        }

        # Structure of the data-set
        a <- factor(rep(x=c(1:n_i), each=n_ij, times=n_j))
        b <- factor(rep(x=c(1:n_j), each=n_i*n_ij))

        # Dataset
        blockinter <- na.omit(data.frame(y_ijk, a=factor(a), b=factor(b)))

        # print(blockinter)
        # str(blockinter)

        blockinter$ab <- interaction(blockinter$a, blockinter$b)

        dropout <- rbinom(n=unique(blockinter$ab), size=1, prob=p_ab)

        ab <- unique(blockinter$ab)


        drop_dat <- data.frame(ab, dropout)

        out <- full_join(blockinter, drop_dat, by = "ab") %>%
                filter(dropout==0) %>%
                select(y_ijk, a, b)


        return(out)
}


# Data Sampling
dat_c2 <- c2_ab_e_drop(mu=-100, n_i=3, n_j=4, n_ij=5,
                       var_i=30, var_j=20, var_ij=10, var_ijk=5,
                       p_e=0, p_ab=0)

dat_c2_new <- c2_ab_e_drop(mu=-100, n_i=3, n_j=4, n_ij=5,
                           var_i=30, var_j=20, var_ij=10, var_ijk=5,
                           p_e=0, p_ab=0)
# Fitting the model
lmer_fit_c2 <- lmer(y_ijk ~ 1 + (1|a) + (1|b) + (1|a:b), dat_c2)

# Run the PI
system.time(pi_calib_c2 <- lmer_pi(model=lmer_fit_c2, m=1, alpha=0.05, traceplot=TRUE))
pi_calib_c2

range(dat_c2$y_ijk)
range(dat_c2_new$y_ijk)





