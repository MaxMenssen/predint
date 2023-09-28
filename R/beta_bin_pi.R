

#' Prediction intervals for beta-binomial data
#'
#' \code{beta_bin_pi()} calculates bootstrap calibrated prediction intervals for
#' beta-binomial data
#'
#' @param histdat a \code{data.frame} with two columns (number of successes and
#' number of failures) containing the historical data
#' @param newdat a \code{data.frame} with two columns (number of successes and
#' number of failures) containing the future data
#' @param newsize a vector containing the future cluster sizes
#' @param alternative either "both", "upper" or "lower". \code{alternative}
#' specifies if a prediction interval or an upper or a lower prediction limit
#' should be computed
#' @param alpha defines the level of confidence (1-\code{alpha})
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
#' \deqn{[l,u]_m = n^*_m \hat{\pi} \pm q^{calib} \hat{se}(Y_m - y^*_m)}
#'
#'where
#'
#' \deqn{\hat{se}(Y_m - y^*_m) = \sqrt{n^*_m \hat{\pi} (1- \hat{\pi}) [1 + (n^*_m -1) \hat{\rho}] +
#' [\frac{n^{*2}_m \hat{\pi} (1- \hat{\pi})}{\sum_h n_h} + \frac{\sum_h n_h -1}{\sum_h n_h} n^{*2}_m \hat{\pi} (1- \hat{\pi}) \hat{\rho}]}}
#'
#' with \eqn{n^*_m} as the number of experimental units in the future clusters,
#' \eqn{\hat{\pi}} as the estimate for the binomial proportion obtained from the
#' historical data, \eqn{q^{calib}} as the bootstrap-calibrated coefficient,
#'  \eqn{\hat{\rho}} as the estimate for the intra class correlation (Lui et al. 2000)
#' and \eqn{n_h} as the number of experimental units per historical cluster. \cr
#'
#' If \code{algorithm} is set to "MS22mod", both limits of the prediction interval
#' are calibrated independently from each other. The resulting prediction interval
#' is given by
#'
#' \deqn{[l,u]_m = \big[n^*_m \hat{\pi} - q^{calib}_l \hat{se}(Y_m - y^*_m), \quad n^*_m \hat{\pi} + q^{calib}_u \hat{se}(Y_m - y^*_m) \big]}
#'
#' Please note, that this modification does not affect the calibration procedure, if only
#' prediction limits are of interest.
#'
#' @return \code{beta_bin_pi} returns an object of class \code{c("predint", "betaBinomialPI")}
#' with prediction intervals or limits in the first entry (\code{$prediction}).
#'
#'
#' @export
#'
#' @importFrom graphics abline lines
#'
#' @references
#' Lui et al. (2000): Confidence intervals for the risk ratio
#' under cluster sampling based on the beta-binomial model. Statistics in Medicine. \cr
#' \doi{10.1002/1097-0258(20001115)19:21<2933::AID-SIM591>3.0.CO;2-Q}
#'
#' Menssen and Schaarschmidt (2022): Prediction intervals for all of M future
#' observations based on linear random effects models. Statistica Neerlandica.
#'  \doi{10.1111/stan.12260}
#'
#'
#' @examples
#'
#' # Prediction interval
#' pred_int <- beta_bin_pi(histdat=mortality_HCD, newsize=40, nboot=100)
#' summary(pred_int)
#'
#' # Upper prediction bound
#' pred_u <- beta_bin_pi(histdat=mortality_HCD, newsize=40, alternative="upper", nboot=100)
#' summary(pred_u)
#'
#' # Please note that nboot was set to 100 in order to decrease computing time
#' # of the example. For a valid analysis set nboot=10000.
#'
#'
beta_bin_pi <- function(histdat,
                        newdat=NULL,
                        newsize=NULL,
                        alternative="both",
                        alpha=0.05,
                        nboot=10000,
                        delta_min=0.01,
                        delta_max=10,
                        tolerance = 1e-3,
                        traceplot=TRUE,
                        n_bisec=30,
                        algorithm="MS22mod"){

# -------------------------------------------------------------------------


        # Relationship between newdat and newsize
        if(is.null(newdat) & is.null(newsize)){
                stop("newdat and newsize are both NULL")
        }

        if(!is.null(newdat) & !is.null(newsize)){
                stop("newdat and newsize are both defined")
        }

        ### historical data
        if(is.data.frame(histdat)==FALSE){
                stop("histdat is not a data.frame")
        }

        if(ncol(histdat) != 2){
                stop("histdat has to have two columns (success, failure)")
        }

        # If all success or failures are 0, stop
        if(all(histdat[,1]==0) || all(histdat[,2]==0)){
                stop("all success or all failure are 0")
        }

        if(isTRUE(alternative!="both" && alternative!="lower" && alternative!="upper")){
                stop("alternative must be either both, lower or upper")
        }

        #-----------------------------------------------------------------------

        # If newsize is given
        if(!is.null(newsize)){

                # size must be integer
                if(!isTRUE(all(newsize == floor(newsize)))){
                        stop("'newsize' must contain integer values only")
                }

                if(length(newsize) > nrow(histdat)){
                        warning("The calculation of a PI for more future than historical observations is not recommended")
                }
        }

        # If an actual data set is given
        if(!is.null(newdat)){
                if(is.data.frame(newdat)==FALSE){
                        stop("newdat is not a data.frame")
                }

                if(ncol(newdat) != 2){
                        stop("newdat has to have two columns (success, failure)")
                }

                # both columns must be integer
                if(!isTRUE(all(newdat[,1] == floor(newdat[,1])))){
                        stop("(newdat[,1] must contain integer values only")
                }

                # both columns must be integer
                if(!isTRUE(all(newdat[,2] == floor(newdat[,2])))){
                        stop("(newdat[,2] must contain integer values only")
                }

                if(nrow(newdat) > nrow(histdat)){
                        warning("The calculation of a PI for more future than historical observations is not recommended")
                }
        }


        #-----------------------------------------------------------------------
        ### Some historical parameters

        # Historical pi and rho
        pi_rho_hat <- unname(pi_rho_est(histdat[,1:2]))

        # Overall pi
        pi_hat <- pi_rho_hat[1]

        # Overall rho
        rho_hat <- pi_rho_hat[2]

        # If rho_hat <= 0, adjust it
        if(rho_hat <= 0){

                rho_hat <- 1e-5
                # newdat$rho_hat <- unname(rho_hat)

                warning("historical data is underdispersed (rho_hat <= 0), \n  rho_hat was set to 0.00001")
        }

        #-----------------------------------------------------------------------
        ### Calculate the uncalibrated PI (only as a base for the bootstrap)

        # If newdat is given
        if(!is.null(newdat)){

                pi_init <- bb_pi(newsize = newdat[,1] + newdat[,2],
                                 histsize = histdat[,1] + histdat[,2],
                                 pi = pi_hat,
                                 rho = rho_hat,
                                 alternative = alternative)
        }

        # If new offset is given
        if(!is.null(newsize)){
                pi_init <- bb_pi(newsize = newsize,
                                 histsize = histdat[,1] + histdat[,2],
                                 pi = pi_hat,
                                 rho = rho_hat,
                                 alternative = alternative)
        }


        #-----------------------------------------------------------------------
        ### Bootstrap

        # Do the bootstrap
        bs_data <- boot_predint(pred_int=pi_init,
                                nboot=nboot)

        # Get bootstrapped future obs.
        bs_futdat <- bs_data$bs_futdat

        bs_y_star <- lapply(X=bs_futdat,
                            FUN=function(x){x$succ})

        # Get bootstrapped historical obs
        bs_histdat <- bs_data$bs_histdat

        #-----------------------------------------------------------------------
        ### Define the input lists for bisection (y_star_hat_m and pred_se_m)

        # Fit the initial model to the bs. hist. obs
        bs_hist_pi_rho <- lapply(X=bs_histdat,
                              FUN=function(x){
                                      pi_rho_hat_bs <- pi_rho_est(x[,1:2])
                                      return(pi_rho_hat_bs)
                              })


        # Get the bs proportion
        bs_pi_hat <- lapply(X=bs_hist_pi_rho,
                            FUN=function(x){
                                    return(unname(x[1]))
                            })

        # Get the bs dispersion parameter
        bs_rho_hat <- lapply(X=bs_hist_pi_rho,
                             FUN=function(x){
                                     return(unname(x[2]))
                             })

        # Get a vector for newsize (if newdat is defined)
        if(!is.null(newdat)){
                newsize <- newdat[,1] + newdat[,2]
        }

        # Total number of individuals in the hist. data
        hist_n_total <- sum(histdat[,1] + histdat[,2])

        # Calculate the prediction SE
        pred_se_fun <- function(n_star_m, rho_hat, pi_hat, n_hist_sum){

                rho_hat_adj <- max(1e-5, rho_hat)

                # Variance of fut. random variable
                var_y_m <- n_star_m * pi_hat * (1-pi_hat) * (1 + (n_star_m - 1) * rho_hat_adj)

                # Variance of fut. expectation
                var_y_star_hat_m <- (n_star_m^2 * pi_hat * (1-pi_hat)) / n_hist_sum +
                        (n_hist_sum-1) / n_hist_sum * n_star_m^2 * pi_hat * (1-pi_hat) * rho_hat_adj

                # Prediction SE
                pred_se <- sqrt(var_y_m + var_y_star_hat_m)

                return(pred_se)
        }

        pred_se_m_list <- mapply(FUN=pred_se_fun,
                                 rho_hat=bs_rho_hat,
                                 pi_hat=bs_pi_hat,
                                 MoreArgs = list(n_star_m=newsize,
                                                 n_hist_sum=hist_n_total),
                                 SIMPLIFY=FALSE)

        # print(pred_se_m_list)

        # Calculate the expected future observations
        y_star_hat_fun <- function(pi_hat, n_star_m){

                out <- pi_hat * n_star_m
                return(out)
        }

        y_star_hat_m_list <- mapply(FUN = y_star_hat_fun,
                                    pi_hat = bs_pi_hat,
                                    MoreArgs = list(n_star_m=newsize),
                                    SIMPLIFY=FALSE)

        #-----------------------------------------------------------------------
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

        #-----------------------------------------------------------------------

        ### Calculate the prediction limits

        out <- bb_pi(newsize = newsize,
                     newdat = newdat,
                     histsize = histdat[,1] + histdat[,2],
                     histdat = histdat,
                     pi = pi_hat,
                     rho = rho_hat,
                     q=quant_calib,
                     alternative=alternative,
                     algorithm=algorithm)

        attr(out, "alpha") <- alpha

        return(out)
}
