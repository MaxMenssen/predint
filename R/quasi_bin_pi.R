

#' Prediction intervals for quasi-binomial data
#'
#' quasi_bin_pi calculates bootstrap calibrated prediction intervals for binomial
#' data with constant overdispersion (quasi-binomial assumption).
#'
#' @param histdat a \code{data.frame} with two columns (success and failures) containing the historical data
#' @param newdat a \code{data.frame} with two columns (success and failures) containing the future data
#' @param newsize a vector containing the future cluster sizes
#' @param alternative either "both", "upper" or "lower". \code{alternative}
#'  specifies if a prediction interval or an upper or a lower prediction limit
#'  should be computed
#' @param alpha defines the level of confidence (1-alpha)
#' @param nboot number of bootstraps
#' @param delta_min lower start value for bisection
#' @param delta_max upper start value for bisection
#' @param tolerance tolerance for the coverage probability in the bisection
#' @param traceplot plot for visualization of the bisection process
#' @param n_bisec maximal number of bisection steps
#' @param algorithm either "MS22" or "MS22mod" (see details)
#'
#' @details This function returns bootstrap-calibrated prediction intervals as well as
#' lower or upper prediction limits.
#'
#' If \code{algorithm} is set to "MS22", both limits of the prediction interval
#' are calibrated simultaniously using the algorithm described in Menssen and
#' Schaarschmidt (2022), section 3.2.4. The calibrated prediction interval is given
#' as
#'
#' \deqn{[l,u]_m = n^*_m \hat{\pi} \pm q^{calib} \sqrt{n^*_m \hat{\pi} (1- \hat{\pi}) (1+
#' \frac{n^*_m}{\sum_h n_h})}}
#'
#' with \eqn{n^*_m} as the number of experimental units in the future clusters,
#' \eqn{\hat{\pi}} as the estimate for the binomial proportion obtained from the
#' historical data, \eqn{q^{calib}} as the bootstrap-calibrated coefficient,
#'  \eqn{\hat{\phi}} as the estimate for the dispersion parameter
#' and \eqn{n_h} as the number of experimental units per historical cluster. \cr
#'
#' If \code{algorithm} is set to "MS22mod", both limits of the prediction interval
#' are calibrated independently from each other. The resulting prediction interval
#' is given by
#'
#' \deqn{[l,u] = [n^*_m \hat{\pi} - q^{calib}_l \sqrt{n^*_m \hat{\pi} (1- \hat{\pi}) (1+
#' \frac{n^*_m}{\sum_h n_h})},
#' n^*_m \hat{\pi} + q^{calib}_u \sqrt{n^*_m \hat{\pi} (1- \hat{\pi}) (1+
#' \frac{n^*_m}{\sum_h n_h})}]}
#'
#' Please note, that this modification does not affect the calibration procedure, if only
#' prediction limits are of interest.
#'
#' @return \code{quasi_bin_pi} returns an object of class \code{c("predint", "quasiBinomialPI")}
#' with prediction intervals or limits in the first entry (\code{$prediction}).
#'
#'@references Menssen and Schaarschmidt (2022): Prediction intervals for all of M future
#' observations based on linear random effects models. Statistica Neerlandica,
#'  \doi{10.1111/stan.12260}
#'
#' @export
#'
#' @importFrom stats glm quasibinomial coef
#'
#' @examples
#' # Prediction interval using qb_dat2 as future data
#' quasi_bin_pi(histdat=qb_dat1, newdat=qb_dat2, nboot=100)
#'
#' # Upper prediction bound for m=3 future observations
#' # that are based on cluster sizes 40, 50, 60 respectively
#' quasi_bin_pi(histdat=qb_dat1, newsize=c(40, 50, 60), alternative="upper", nboot=100)
#'
#' # Please note that nboot was set to 100 in order to decrease computing time
#' # of the example. For a valid analysis set nboot=10000.
#'
quasi_bin_pi <- function(histdat,
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


        # alternative must be defined
        if(isTRUE(alternative!="both" && alternative!="lower" && alternative!="upper")){
                stop("alternative must be either both, lower or upper")
        }

        #-----------------------------------------------------------------------

        ### Actual data

        # either newdat or newsize must be given
        if(!is.null(newdat) && !is.null(newsize)){
                stop("either newdat or newsize must be given, but not both")
        }


        # If newsize is given
        if(!is.null(newsize)){

                # size must be integer
                if(!isTRUE(all(newsize == floor(newsize)))){
                        stop("'newsize' must contain integer values only")
                }

                if(length(newsize) > nrow(histdat)){
                        warning("The calculation of a PI for more future than historical observations is not recommended")
                }

                # total <- newsize
                # newdat <- as.data.frame(total)
                # m <- nrow(newdat)
        }


        # If an actual data set is given
        else{
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
                        stop("'(newdat[,2] must contain integer values only")
                }

                if(nrow(newdat) > nrow(histdat)){
                        warning("The calculation of a PI for more future than historical observations is not recommended")
                }

                # m <- nrow(newdat)
                # newdat$total <- newdat[,1 ]+ newdat[,2]

        }

        #-----------------------------------------------------------------------
        ### Model fit


        model <- glm(cbind(histdat[,1], histdat[,2]) ~ 1,
                     family=quasibinomial(link="logit"))

        #-----------------------------------------------------------------------
        ### Phi and pi

        # Historical phi
        hist_phi <- summary(model)$dispersion

        # If historical phi <= 1, adjust it
        if(hist_phi <= 1){

                hist_phi <- 1.001

                warning("historical data is underdispersed (hist_phi <= 1), \n  dispersionparameter was set to 1.001")
        }


        # Historical pi
        hist_prob <- exp(unname(coef(model)))/(1+exp(unname(coef(model))))

        #-----------------------------------------------------------------------
        ### Calculate the uncalibrated PI (only as a base for bootstrap)

        # If newdat is given
        if(!is.null(newdat)){

                pi_init <- qb_pi(newsize = newdat[,1] + newdat[,2],
                                 histsize = histdat[,1] + histdat[,2],
                                 pi = hist_prob,
                                 phi = hist_phi,
                                 alternative = alternative)
        }

        # If new offset is given
        if(!is.null(newsize)){
                pi_init <- qb_pi(newsize = newsize,
                                 histsize = histdat[,1] + histdat[,2],
                                 pi = hist_prob,
                                 phi = hist_phi,
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
        bs_hist_glm <- lapply(X=bs_histdat,
                              FUN=function(x){
                                      fit <- glm(cbind(x[,1], x[,2]) ~ 1,
                                                 family=quasibinomial(link="logit"))
                                      return(fit)
                              })


        # Get the bs proportion
        bs_pi_hat_logit <- lapply(X=bs_hist_glm,
                                FUN=function(x){
                                        return(unname(coef(x)))
                                })

        bs_pi_hat <- lapply(X=bs_pi_hat_logit,
                                  FUN=function(x){
                                          return(exp(x) / (1+exp(x)))
                                  })

        # Get the bs dispersion parameter
        bs_phi_hat <- lapply(X=bs_hist_glm,
                             FUN=function(x){
                                     return(summary(x)$dispersion)
                             })

        # Get a vector for newoffset (if newdat is defined)
        if(!is.null(newdat)){
                newsize <- newdat[,1] + newdat[,2]
        }

        # Total number of individuals in the hist. data
        hist_n_total <- sum(histdat[,1] + histdat[,2])

        # Calculate the prediction SE
        pred_se_fun <- function(n_star_m, phi_hat, pi_hat, n_hist_sum){

                # Variance of fut. random variable
                var_y_m <- n_star_m * phi_hat * pi_hat * (1-pi_hat)

                # Variance of fut. expectation
                var_y_star_hat_m <- n_star_m^2 * phi_hat * pi_hat * (1-pi_hat) * 1/n_hist_sum

                # Prediction SE
                pred_se <- sqrt(var_y_m + var_y_star_hat_m)

                return(pred_se)
        }

        pred_se_m_list <- mapply(FUN=pred_se_fun,
                                 phi_hat=bs_phi_hat,
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

        # print(y_star_hat_m_list)

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

        #-----------------------------------------------------------------------

        ### Calculate the prediction limits

        out <- qb_pi(newsize = newsize,
                     newdat = newdat,
                     histsize = histdat[,1] + histdat[,2],
                     histdat = histdat,
                     pi = hist_prob,
                     phi = hist_phi,
                     q=quant_calib,
                     alternative=alternative,
                     algorithm=algorithm)

        attr(out, "alpha") <- alpha

        return(out)
}




