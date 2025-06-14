#------------------------------------------------------------------------------
#----------------------- Quasi poisson PI -------------------------------------
#------------------------------------------------------------------------------

#' Prediction intervals for quasi-Poisson data
#'
#' \code{quasi_pois_pi()} calculates bootstrap calibrated prediction intervals for Poisson
#' data with constant overdispersion (quasi-Poisson).
#'
#' @param histdat a \code{data.frame} with two columns. The first has to contain
#' the historical observations. The second has to contain the number of experimental
#' units per study (offsets).
#' @param newdat a \code{data.frame} with two columns. The first has to contain
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
#' \deqn{[l,u]_m = n^*_m \hat{\lambda} \pm q^{calib} \sqrt{n^*_m \hat{\phi} \hat{\lambda} +
#'  \frac{n^{*2}_m \hat{\phi} \hat{\lambda}}{\sum_h n_h}}}
#'
#' with \eqn{n^*_m} as the number of experimental units in the future clusters,
#' \eqn{\hat{\lambda}} as the estimate for the Poisson mean obtained from the
#' historical data, \eqn{q^{calib}} as the bootstrap-calibrated coefficient,
#' \eqn{\hat{\phi}} as the estimate for the dispersion parameter
#' and \eqn{n_h} as the number of experimental units per historical cluster. \cr
#'
#' If \code{algorithm} is set to "MS22mod", both limits of the prediction interval
#' are calibrated independently from each other. The resulting prediction interval
#' is given by
#'
#' \deqn{[l,u] = \Big[n^*_m \hat{\lambda} - q^{calib}_l \sqrt{n^*_m \hat{\phi} \hat{\lambda} +
#'  \frac{n^{*2}_m \hat{\phi} \hat{\lambda}}{\sum_h n_h}}, \quad
#'  n^*_m \hat{\lambda} + q^{calib}_u \sqrt{n^*_m \hat{\phi} \hat{\lambda} +
#'  \frac{n^{*2}_m \hat{\phi} \hat{\lambda}}{\sum_h n_h}} \Big]}
#'
#' Please note, that this modification does not affect the calibration procedure, if only
#' prediction limits are of interest.
#'
#' @return \code{quasi_pois_pi} returns an object of class \code{c("predint", "quasiPoissonPI")}
#' with prediction intervals or limits in the first entry (\code{$prediction}).
#'
#'
#' @references Menssen et al. (2025): Prediction Intervals for Overdispersed Poisson
#' Data and Their Application in Medical and Pre-Clinical Quality Control.
#' Pharmaceutical Statistics \doi{10.1002/pst.2447}
#'
#' @export
#'
#' @importFrom stats glm quasipoisson coef
#'
#' @examples
#' #' # Historical data
#' qp_dat1
#'
#' # Future data
#' qp_dat2
#'
#' # Pointwise prediction interval
#' pred_int <- quasi_pois_pi(histdat=ames_HCD, newoffset=3, nboot=100)
#' summary(pred_int)
#'
#' # Pointwise upper prediction
#' pred_u <- quasi_pois_pi(histdat=ames_HCD, newoffset=3, alternative="upper", nboot=100)
#' summary(pred_u)
#'
#' # Please note that nboot was set to 100 in order to decrease computing time
#' # of the example. For a valid analysis set nboot=10000.
#'
quasi_pois_pi <- function(histdat,
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

                if(any((colnames(histdat) == colnames(newdat))==FALSE)){
                        stop("histdat and newdat have to have the same colnames")
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

        model <- glm(histdat[,1]~1,
                     family=quasipoisson(link="log"),
                     offset = log(histdat[,2]))

        # Historical lambda
        lambda_hat <- exp(unname(coef(model)))

        # Historical phi
        phi_hat<- summary(model)$dispersion

        # If historical phi <= 1, adjust it
        if(phi_hat <= 1){

                phi_hat <- 1.001

                warning("historical data is underdispersed (phi_hat <= 1), \n  dispersionparameter was set to 1.001")
        }

        #-----------------------------------------------------------------------
        ### Calculate the uncalibrated PI

        # If newdat is given
        if(!is.null(newdat)){

                pi_init <- qp_pi(newoffset = newdat[,2],
                                 histoffset = histdat[,2],
                                 lambda = lambda_hat,
                                 phi = phi_hat,
                                 alternative = alternative)
        }

        # If new offset is given
        if(!is.null(newoffset)){
                pi_init <- qp_pi(newoffset = newoffset,
                                 histoffset = histdat[,2],
                                 lambda = lambda_hat,
                                 phi = phi_hat,
                                 alternative = alternative)
        }

        #-----------------------------------------------------------------------
        ### Bootstrap

        # Do the bootstrap
        bs_data <- boot_predint(pred_int=pi_init,
                                nboot=nboot,
                                adjust=adjust)

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

        out <- qp_pi(newoffset = newoffset,
                     newdat = newdat,
                     histoffset = histdat[,2],
                     histdat = histdat,
                     lambda = lambda_hat,
                     phi = phi_hat,
                     q=quant_calib,
                     alternative=alternative,
                     algorithm=algorithm)

        attr(out, "alpha") <- alpha

        return(out)
}







