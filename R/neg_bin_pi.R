#' Prediction intervals for negative-binomial data
#'
#' \code{neg_bin_pi()} calculates bootstrap calibrated prediction intervals for
#' negative-binomial data.
#'
#' @param histdat a \code{data.frame} with two columns. The first has to contain
#' the historical observations. The second has to contain the number of experimental
#' units per study (offsets).
#' @param newdat \code{data.frame} with two columns. The first has to contain
#' the future observations. The second has to contain the number of experimental
#' units per study (offsets).
#' @param newoffset vector with future number of experimental units per historical
#' study.
#' @param alternative either "both", "upper" or "lower".
#' \code{alternative} specifies if a prediction interval or
#' an upper or a lower prediction limit should be computed
#' @param adjust specifies if simultaneous prediction should be done for several
#' control groups of different studies (\code{between}), or for the outcome of
#' the current control and some treatment groups \code{within} the same trial
#' @param alpha defines the level of confidence (\eqn{1-\alpha})
#' @param nboot number of bootstraps
#' @param delta_min lower start value for bisection
#' @param delta_max upper start value for bisection
#' @param tolerance tolerance for the coverage probability in the bisection
#' @param traceplot if \code{TRUE}: Plot for visualization of the bisection process
#' @param n_bisec maximal number of bisection steps
#' @param algorithm either "MS22" or "MS22mod" (see details)
#'
#' @details This function returns bootstrap-calibrated prediction intervals as well as
#' lower or upper prediction limits.
#'
#' If \code{algorithm} is set to "MS22", both limits of the prediction interval
#' are calibrated simultaneously using the algorithm described in Menssen and
#' Schaarschmidt (2022), section 3.2.4. The calibrated prediction interval is given
#' as
#'
#' \deqn{[l,u]_m = n^*_m \hat{\lambda} \pm q \sqrt{n^*_m
#' \frac{\hat{\lambda} + \hat{\kappa} \bar{n} \hat{\lambda}}{\bar{n} H} +
#' (n^*_m \hat{\lambda} + \hat{\kappa} n^{*2}_m \hat{\lambda}^2)
#' }}
#'
#' with \eqn{n^*_m} as the number of experimental units in the future clusters,
#' \eqn{\hat{\lambda}} as the estimate for the Poisson mean obtained from the
#' historical data, \eqn{\hat{\kappa}} as the estimate for the dispersion parameter,
#' \eqn{n_h} as the number of experimental units per historical cluster and
#' \eqn{\bar{n}=\sum_h^{n_h} n_h / H}.  \cr
#'
#' If \code{algorithm} is set to "MS22mod", both limits of the prediction interval
#' are calibrated independently from each other. The resulting prediction interval
#' is given by
#'
#' \deqn{[l,u] = \Big[n^*_m \hat{\lambda} - q^{calib}_l \sqrt{n^*_m
#' \frac{\hat{\lambda} + \hat{\kappa} \bar{n} \hat{\lambda}}{\bar{n} H} +
#' (n^*_m \hat{\lambda} + \hat{\kappa} n^{*2}_m \hat{\lambda}^2)}, \quad
#'  n^*_m \hat{\lambda} + q^{calib}_u \sqrt{n^*_m
#' \frac{\hat{\lambda} + \hat{\kappa} \bar{n} \hat{\lambda}}{\bar{n} H} +
#' (n^*_m \hat{\lambda} + \hat{\kappa} n^{*2}_m \hat{\lambda}^2)
#' } \Big]}
#'
#' Please note, that this modification does not affect the calibration procedure, if only
#' prediction limits are of interest.
#'
#' @return \code{neg_bin_pi()} returns an object of class \code{c("predint", "negativeBinomialPI")}
#' with prediction intervals or limits in the first entry (\code{$prediction}).
#'
#' @export
#'
#' @importFrom MASS glm.nb
#'
#' @references Menssen and Schaarschmidt (2022): Prediction intervals for all of M future
#' observations based on linear random effects models. Statistica Neerlandica,
#'  \doi{10.1111/stan.12260}
#'
#' @examples
#' # HCD from the Ames test
#' ames_HCD
#'
#' # Pointwise prediction interval for one future number of revertant colonies
#' # obtained in three petridishes
#' pred_int <- neg_bin_pi(histdat=ames_HCD, newoffset=3, nboot=100)
#' summary(pred_int)
#'
#' # Simultaneous prediction interval for the numbers of revertant colonies obtained in
#' # the control and three treatment groups of a future trial
#' pred_int_w <- neg_bin_pi(histdat=ames_HCD, newoffset=c(3, 3, 3, 3), adjust="within", nboot=100)
#' summary(pred_int_w)
#'
#' # Please note that nboot was set to 100 in order to decrease computing time
#' # of the example. For a valid analysis set nboot=10000.
#'
neg_bin_pi <- function(histdat,
                       newdat=NULL,
                       newoffset=NULL,
                       alternative="both",
                       adjust="within",
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

        # if(!(is.numeric(histdat[,1]) | is.integer(histdat[,1]))){
        #         stop("At least one variable in histdat is neither integer or numeric")
        # }

        if(!(is.numeric(histdat[,2]) | is.integer(histdat[,2]))){
                stop("Offset variable in histdat is neither integer or numeric")
        }

        if(!isTRUE(all(histdat[,1] == floor(histdat[,1])))){
                stop("the historical observations have to be integers")
        }

        if(all(histdat[,1] == 0)){

                stop("All observations are zero")
        }
        #-----------------------------------------------------------------------
        ### Current data

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

                # if(!(is.numeric(newdat[,1]) | is.integer(newdat[,1]))){
                #         stop("At least one variable in newdat is neither integer or numeric")
                # }

                if(!(is.numeric(newdat[,2]) | is.integer(newdat[,2]))){
                        stop("Offset variable in newdat is neither integer or numeric")
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
                                nboot=nboot,
                                adjust=adjust)

        # print(bs_data)
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
        bs_hist_glm_na <- lapply(X=bs_histdat,
                              FUN=function(x){
                                      fit <- try(suppressWarnings(glm.nb(x[,1]~1 + offset(log(x[,2])))))

                                      if(all(class(fit)=="try-error")){
                                              return(NA)
                                      }
                                      else{
                                              return(fit)
                                      }
                              })

        # print(bs_hist_glm_na)

        # Omit all fits with convergence problems
        bs_hist_glm <- bs_hist_glm_na[!is.na(bs_hist_glm_na)]

        if((length(bs_hist_glm_na) - length(bs_hist_glm)) > 0){

                n_na <- length(bs_hist_glm_na) - length(bs_hist_glm)
                warning(paste("glm.nb could not be fit to", n_na, "bs-data sets"))

        }

        # Get the bs Poisson mean
        bs_lambda_hat <- lapply(X=bs_hist_glm,
                                FUN=function(x){
                                        return(exp(unname(coef(x))))
                                })

        # Get the bs dispersion parameter (kappa)
        bs_kappa_hat <- lapply(X=bs_hist_glm,
                             FUN=function(x){
                                     return(max(1/x$theta, 0.0001))
                             })

        # Get a vector for newoffset (if newdat is defined)
        if(!is.null(newdat)){
                newoffset <- newdat[,2]
        }


        # Calculate the prediction SE
        pred_se_fun <- function(n_star_m, kappa_hat, lambda_hat, n_hist_mean, H){

                # var for future random variable
                var_y_star <- n_star_m * lambda_hat + kappa_hat * n_star_m^2 * lambda_hat^2

                # var for the expected future observations
                var_y_star_hat <- n_star_m^2 * (lambda_hat + kappa_hat * n_hist_mean * lambda_hat) / (n_hist_mean * H)

                # SE for prediction
                pred_se <- sqrt(var_y_star + var_y_star_hat)
                return(pred_se)
        }

        pred_se_m_list <- mapply(FUN=pred_se_fun,
                                 kappa_hat=bs_kappa_hat,
                                 lambda_hat=bs_lambda_hat,
                                 MoreArgs = list(n_star_m=newoffset,
                                                 n_hist_mean=mean(pi_init$histoffset),
                                                 H=length(pi_init$histoffset)),
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

# set.seed(123)
# predint::rnbinom(n=10, lambda=50, kappa=0.04)
# nb_dat <- predint::rnbinom(n=40000, lambda=50, kappa=0.04, offset=round(runif(n=40000, 1, 4)))
#
#
# nb_pi_test <- neg_bin_pi(histdat=nb_dat,
#            newdat = rnbinom(n=10, lambda=50, kappa=0.04, offset=round(runif(n=10, 1, 4))))
# nb_pi_test
# plot(nb_pi_test)
