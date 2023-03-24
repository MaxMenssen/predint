neg_bin_pi <- function(histdat,
                       newdat=NULL,
                       newoffset=NULL,
                       alternative="both",
                       alpha=0.05,
                       nboot=10000,
                       delta_min=0.01,
                       delta_max=10,
                       tolerance = 1e-3,
                       traceplot=TRUE,
                       n_bisec=30,
                       algorithm="MS22mod"){



        #-----------------------------------------------------------------------
        ### historical data

        if(is.data.frame(histdat)==FALSE){
                stop("histdat is not a data.frame")
        }

        if(ncol(histdat) != 2){
                stop("histdat has to have two columns (observations and number of exp. units)")
        }

        if(!(is.numeric(histdat[,1]) | is.integer(histdat[,1]))){
                stop("At least one variable in histdat is neither integer or numeric")
        }

        if(!(is.numeric(histdat[,2]) | is.integer(histdat[,2]))){
                stop("At least one variable in histdat is neither integer or numeric")
        }

        if(!isTRUE(all(histdat[,1] == floor(histdat[,1])))){
                stop("the historical observations have to be integers")
        }

        #-----------------------------------------------------------------------
        ### Actual data

        # Relationship between newdat and newoffset
        if(is.null(newdat) & is.null(newoffset)){
                stop("newdat and newoffset are both NULL")
        }

        if(!is.null(newdat) & !is.null(newoffset)){
                stop("newdat and newoffset are both defined")
        }

        # If newdat is defined
        if(is.null(newdat) == FALSE){
                if(is.data.frame(newdat)==FALSE){
                        stop("newdat is not a data.frame")
                }

                if(ncol(newdat) != 2){
                        stop("newdat has to have two columns (observations and number of exp. units")
                }

                if(!(is.numeric(newdat[,1]) | is.integer(newdat[,1]))){
                        stop("At least one variable in newdat is neither integer or numeric")
                }

                if(!(is.numeric(newdat[,2]) | is.integer(newdat[,2]))){
                        stop("At least one variable in newdat is neither integer or numeric")
                }

                if(nrow(newdat) > nrow(histdat)){
                        warning("The calculation of a PI for more future than historical observations is not recommended")
                }

                if(!isTRUE(all(newdat[,1] == floor(newdat[,1])))){
                        stop("the future observations have to be integers")
                }
        }

        # If newoffset is defined
        if(is.null(newdat) & is.null(newoffset)==FALSE){

                # newoffset must be integer or
                if(!(is.numeric(newoffset) | is.integer(newoffset))){
                        stop("newoffset is neither integer or numeric")
                }

                if(length(newoffset) > nrow(histdat)){
                        warning("The calculation of a PI for more future than historical observations is not recommended")
                }

        }

        # alternative must be defined
        if(isTRUE(alternative!="both" && alternative!="lower" && alternative!="upper")){
                stop("alternative must be either both, lower or upper")
        }

        # algorithm must be set properly
        if(algorithm != "MS22"){
                if(algorithm != "MS22mod"){
                        stop("algoritm must be either MS22 of MS22mod")
                }
        }

        #-----------------------------------------------------------------------

        ### Historical numbers of observations
        hist_n_total <- sum(histdat[,2])

        #-----------------------------------------------------------------------
        ### Model and parameters (phi_hat, lambda_hat)

        model <- glm.nb(histdat[,1]~ 1 + offset(log(histdat[,2])))

        # Historical lambda
        lambda_hat <- exp(unname(coef(model)))

        # Historical kappa
        kappa_hat <- 1/model$theta

        # If historical phi <= 1, adjust it
        if(kappa_hat <= 0){

                kappa_hat <- 0.0001

                warning("historical data is underdispersed (kappa_hat <= 0), \n  dispersionparameter was set to 0.0001")
        }

        #-----------------------------------------------------------------------
        ### Calculate the uncalibrated PI

        # If newdat is given
        if(!is.null(newdat)){

                pi_init <- nb_pi(newoffset = newdat[,2],
                                 histoffset = histdat[,2],
                                 lambda = lambda_hat,
                                 kappa = kappa_hat,
                                 alternative = alternative)
        }

        # If new offset is given
        if(!is.null(newoffset)){
                pi_init <- nb_pi(newoffset = newoffset,
                                 histoffset = histdat[,2],
                                 lambda = lambda_hat,
                                 kappa = kappa_hat,
                                 alternative = alternative)
        }

        # print("pi_init")
        # print(str(pi_init))
        #-----------------------------------------------------------------------
        ### Bootstrap

        # Do the bootstrap
        bs_data <- boot_predint(pred_int=pi_init,
                                nboot=nboot)

        # print("bs_data")
        # print(str(bs_data))

        # Get bootstrapped future obs.
        bs_futdat <- bs_data$bs_futdat

        bs_y_star <- lapply(X=bs_futdat,
                            FUN=function(x){x$y})

        # Get bootstrapped historical obs
        bs_histdat <- bs_data$bs_histdat

        #-----------------------------------------------------------------------
        ### Define the input lists for bisection (y_star_hat_m and pred_se_m)

        # Fit the initial model to the bs. hist. obs
        bs_hist_glm <- lapply(X=bs_histdat,
                              FUN=function(x){
                                      fit <- glm(x[,1]~1,
                                                 family=quasipoisson(link="log"),
                                                 offset = log(x[,2]))
                                      return(fit)
                              })

        # Get the bs Poisson mean
        bs_lambda_hat <- lapply(X=bs_hist_glm,
                                FUN=function(x){
                                        return(exp(unname(coef(x))))
                                })

        # Get the bs dispersion parameter
        bs_phi_hat <- lapply(X=bs_hist_glm,
                             FUN=function(x){
                                     return(summary(x)$dispersion)
                             })

        # Get a vector for newoffset (if newdat is defined)
        if(!is.null(newdat)){
                newoffset <- newdat[,2]
        }


        # Calculate the prediction SE
        pred_se_fun <- function(n_star_m, phi_hat, lambda_hat, n_hist_sum){

                # Variance of fut. random variable
                var_y <- n_star_m * phi_hat * lambda_hat

                # Variance of fut. expectation
                var_y_star_hat_m <- n_star_m^2 * phi_hat * lambda_hat * 1/n_hist_sum

                # Prediction SE
                pred_se <- sqrt(var_y + var_y_star_hat_m)

                return(pred_se)
        }

        pred_se_m_list <- mapply(FUN=pred_se_fun,
                                 phi_hat=bs_phi_hat,
                                 lambda_hat=bs_lambda_hat,
                                 MoreArgs = list(n_star_m=newoffset,
                                                 n_hist_sum=hist_n_total),
                                 SIMPLIFY=FALSE)

        # Calculate the expected future observations
        y_star_hat_fun <- function(lambda_hat, n_star_m){

                out <- lambda_hat * n_star_m
                return(out)
        }

        y_star_hat_m_list <- mapply(FUN = y_star_hat_fun,
                                    lambda_hat = bs_lambda_hat,
                                    MoreArgs = list(n_star_m=newoffset),
                                    SIMPLIFY=FALSE)

        #-----------------------------------------------------------------------

        ### Calculation of the calibrated quantile

        # Calibration for of lower prediction limits
        if(alternative=="lower"){

                quant_calib <- bisection(y_star_hat = y_star_hat_m_list,
                                         pred_se = pred_se_m_list,
                                         y_star = bs_y_star,
                                         alternative = alternative,
                                         quant_min = delta_min,
                                         quant_max = delta_max,
                                         n_bisec = n_bisec,
                                         tol = tolerance,
                                         alpha = alpha,
                                         traceplot=traceplot)
        }

        # Calibration for of upper prediction limits
        if(alternative=="upper"){

                quant_calib <- bisection(y_star_hat = y_star_hat_m_list,
                                         pred_se = pred_se_m_list,
                                         y_star = bs_y_star,
                                         alternative = alternative,
                                         quant_min = delta_min,
                                         quant_max = delta_max,
                                         n_bisec = n_bisec,
                                         tol = tolerance,
                                         alpha = alpha,
                                         traceplot=traceplot)
        }


        # Calibration for  prediction intervals
        if(alternative=="both"){

                # Direct implementation of M and S 2021
                if(algorithm=="MS22"){
                        quant_calib <- bisection(y_star_hat = y_star_hat_m_list,
                                                 pred_se = pred_se_m_list,
                                                 y_star = bs_y_star,
                                                 alternative = alternative,
                                                 quant_min = delta_min,
                                                 quant_max = delta_max,
                                                 n_bisec = n_bisec,
                                                 tol = tolerance,
                                                 alpha = alpha,
                                                 traceplot=traceplot)
                }

                # Modified version of M and S 21
                if(algorithm=="MS22mod"){
                        quant_calib_lower <- bisection(y_star_hat = y_star_hat_m_list,
                                                       pred_se = pred_se_m_list,
                                                       y_star = bs_y_star,
                                                       alternative = "lower",
                                                       quant_min = delta_min,
                                                       quant_max = delta_max,
                                                       n_bisec = n_bisec,
                                                       tol = tolerance,
                                                       alpha = alpha/2,
                                                       traceplot=traceplot)

                        quant_calib_upper <- bisection(y_star_hat = y_star_hat_m_list,
                                                       pred_se = pred_se_m_list,
                                                       y_star = bs_y_star,
                                                       alternative = "upper",
                                                       quant_min = delta_min,
                                                       quant_max = delta_max,
                                                       n_bisec = n_bisec,
                                                       tol = tolerance,
                                                       alpha = alpha/2,
                                                       traceplot=traceplot)

                        quant_calib <- c(quant_calib_lower, quant_calib_upper)
                }

        }

        # print(quant_calib)
        # stop(quant_calib)

        #-----------------------------------------------------------------------

        ### Calculate the prediction limits

        out <- nb_pi(newoffset = newoffset,
                     newdat = newdat,
                     histoffset = histdat[,2],
                     histdat = histdat,
                     lambda = lambda_hat,
                     kappa = kappa_hat,
                     q=quant_calib,
                     alternative=alternative,
                     algorithm=algorithm)

        attr(out, "alpha") <- alpha

        return(out)
}

set.seed(123)
predint::rnbinom(n=10, lambda=50, kappa=0.04)
nb_dat <- predint::rnbinom(n=100, lambda=50, kappa=0.04)


nb_pi_test <- neg_bin_pi(histdat=nb_dat,
           newoffset = 1)
nb_pi_test
str(nb_pi_test)
