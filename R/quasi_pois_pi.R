#------------------------------------------------------------------------------
#----------------------- Quasi poisson PI -------------------------------------
#------------------------------------------------------------------------------

#' Prediction intervals for quasi-Poisson data
#'
#' quasi_pois_pi calculates bootstrap calibrated prediction intervals for Poisson
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
#' @param alpha defines the level of confidence (1-alpha)
#' @param nboot number of bootstraps
#' @param delta_min lower start value for bisection
#' @param delta_max upper start value for bisection
#' @param tolerance tolerance for the coverage probability in the bisection
#' @param traceplot plot for visualization of the bisection process
#' @param n_bisec maximal number of bisection steps
#'
#' @details This function returns a bootstrap calibrated prediction interval
#' \deqn{[l,u] = \hat{y} \pm q \sqrt{\hat{var}(\hat{y} - y)}}
#' with \eqn{\hat{y}} as the predicted future observation,
#' \eqn{y} as the observed future observations, \eqn{\sqrt{\hat{var}(\hat{y} - y)}}
#' as the prediction error and \eqn{q} as the bootstrap calibrated coefficient that
#' approximates a quantile of a multivariate normal distribution. \cr
#'
#' @return If \code{newdat} is specified: A \code{data.frame} that contains the future data,
#'  the historical mean (hist_mean), the calibrated coefficient (quant_calib),
#'  the prediction error (pred_se), the prediction interval (lower and upper)
#'  and a statement if the prediction interval covers the future observation (cover).
#'
#'  If \code{m} is specified: A \code{data.frame} that contains the number of future observations (m)
#'  the historical mean (hist_mean), the calibrated coefficient (quant_calib),
#'  the prediction error (pred_se) and the prediction interval (lower and upper).
#'
#'  If \code{alternative} is set to "lower": Lower prediction bounds are computed instead
#'  of a prediction interval.
#'
#'  If \code{alternative} is set to "upper": Upper prediction bounds are computed instead
#'  of a prediction interval.
#'
#'  If \code{traceplot=TRUE}, a graphical overview about the bisection process is given.
#'
#'
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
#' # Prediction interval using bb_dat2 as future data
#' quasi_pois_pi(histdat=data.frame(qp_dat1), newdat=data.frame(qp_dat2), nboot=100)
#'
#' # Upper prediction bound for m=3 future observations
#' quasi_pois_pi(histdat=data.frame(qp_dat1), m=3, alternative="upper", nboot=100)
#'
#' # Please note that nboot was set to 100 in order to decrease computing time
#' # of the example. For a valid analysis set nboot=10000.
#'
quasi_pois_pi <- function(histdat,
                          newdat=NULL,
                          newoffset=NULL,
                          alternative="both",
                          alpha=0.05,
                          nboot=10000,
                          delta_min=0.01,
                          delta_max=10,
                          tolerance = 1e-3,
                          traceplot=TRUE,
                          n_bisec=30){



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

        #-----------------------------------------------------------------------

        ### Historical numbers of observations
        hist_n_total <- sum(histdat[,2])

        #-----------------------------------------------------------------------
        ### Model and parameters (phi_hat, lambda_hat)

        model <- glm(hdat[,1]~1,
                     family=quasipoisson(link="log"),
                     offset = log(hdat[,2]))

        # Historical lambda
        lambda_hat <- exp(unname(coef(model)))

        # Historical phi
        phi_hat<- summary(model)$dispersion

        # If historical phi <= 1, adjust it
        if(lambda_hat <= 1){

                lambda_hat <- 1.001

                warning("historical data is underdispersed (lambda_hat <= 1), \n  dispersionparameter was set to 1.001")
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
                                nboot=nboot)

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

        # Calculation of the calibrated quantile

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


        #-----------------------------------------------------------------------

        ### Calculate the prediction limits

        out <- qp_pi(newoffset = newoffset,
                     histoffset = histdat[,2],
                     lambda = lambda_hat,
                     phi = phi_hat,
                     q=quant_calib,
                     alternative=alternative)

        return(out)
}



### TO DO
# - insert a newdata argument in qp_pi
# - Document the bootstrap and the bisection function
# - Insert several checks in the bisection and coverage_prob functions
# - Check for bugs (phi needs adjustment if smaller than 1)


# hdat <- rqpois(n=30, lambda=10, phi=3, offset=NULL)
# hdat
#
# fdat <- rqpois(n=1, lambda=10, phi=3, offset=NULL)
# fdat
#
# quasi_pois_pi(histdat=hdat,
#               # newoffset = c(1:3),
#               alternative = "both",
#               nboot=1000,
#               newdat=fdat)

#
# test_pi <- qp_pi(newoffset=fdat[,2],
#                  lambda=10,
#                  phi=3,
#                  histoffset=hdat[,2],
#                  q=qnorm(1-0.05/2),
#                  alternative="lower")
#
# boot_dat <- boot_predint(test_pi,
#                          nboot=2)
#
# bdat <- boot_dat$bs_histdat[[1]]
#
# glm(bdat[,1]~1,
#     family=quasipoisson(link="log"),
#     offset = log(bdat[,2]))







