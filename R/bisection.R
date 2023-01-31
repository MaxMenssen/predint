#' Bisection algorithm for bootstrap calibration of prediction intervals
#'
#' This helper function returns a bootstrap calibrated coefficient for the calculation
#' of prediction intervals (and limits).
#'
#' @param y_star_hat a list of length \eqn{B} that contains the expected future observations.
#' Each entry in this list has to be a numeric vector of length \eqn{M}.
#' @param pred_se a list of length \eqn{B} that contains the standard errors of the prediction.
#' Each entry in this list has to be a numeric vector of length \eqn{M}.
#' @param y_star a list of length \eqn{B} that contains the future observations.
#' Each entry in this list has to be a numeric vector of length \eqn{M}.
#' @param alternative either "both", "upper" or "lower".
#' \code{alternative} specifies if a prediction interval or
#' an upper or a lower prediction limit should be computed
#' @param quant_min lower start value for bisection
#' @param quant_max upper start value for bisection
#' @param n_bisec maximal number of bisection steps
#' @param tol tolerance for the coverage probability in the bisection
#' @param alpha defines the level of confidence (\eqn{1-\alpha})
#' @param traceplot if \code{TRUE}: Plot for visualization of the bisection process
#'
#' @details This function is an implementation of the bisection algorithm of Menssen
#' and Schaarschmidt 2022. It returns a calibrated coefficient \eqn{q^{calib}} for the
#' calculation of simultaneous prediction intervals (or limits)
#' \deqn{[l,u] = \hat{y}^*_m  \pm q^{calib} \hat{se}(Y_m - y^*_m)}
#' that cover all of \eqn{m=1, ... , M} future observations. \cr
#'
#' In this notation, \eqn{\hat{y}^*_m} are the expected future observations for each of
#' the \eqn{m} future clusters, \eqn{q^{calib}} is the
#' calibrated coefficient and \eqn{\hat{se}(Y_m - y^*_m)}
#' are the standard errors of the prediction. \cr
#'
#' @return This function returns \eqn{q^{calib}} in the equation above.
#'
#' @references Menssen and Schaarschmidt (2022): Prediction intervals for all of M future
#' observations based on linear random effects models. Statistica Neerlandica. \cr
#'  \doi{10.1111/stan.12260}
#'
#' @export
#'
#'
bisection <- function(y_star_hat,
                      pred_se,
                      y_star,
                      alternative,
                      quant_min,
                      quant_max,
                      n_bisec,
                      tol,
                      alpha,
                      traceplot=TRUE){

        # y_star_hat has to be a list
        if(!is.list(y_star_hat)){
                stop("!is.list(y_star_hat)")
        }

        # pred_se has to be a list
        if(!is.list(pred_se)){
                stop("!is.list(pred_se)")
        }

        # y_star has to be a list
        if(!is.list(y_star)){
                stop("!is.list(y_star)")
        }

        # all three lists have to have the same length
        if(length(unique(c(length(y_star_hat), length(pred_se), length(y_star)))) != 1){
                stop("length(unique(c(length(y_star_hat), length(pred_se), length(y_star)))) != 1")
        }

        # all elements of y_star_hat have to be of length M
        if(length(unique(sapply(y_star_hat, length))) != 1){
                stop("all elements in y_star_hat have to have the same length M")
        }

        # all elements of pred_se have to be of length M
        if(length(unique(sapply(pred_se, length))) != 1){
                stop("all elements in pred_se have to have the same length M")
        }

        # all elements of y_star have to be of length M
        if(length(unique(sapply(y_star, length))) !=1){
                stop("all elements in y_star have to have the same length M")
        }

        # M has to be the same for all three lists
        if(length(unique(unique(sapply(y_star_hat, length)), unique(sapply(pred_se, length)), unique(sapply(y_star, length)))) != 1){
                stop("M differs between y_star_hat, pred_se and y_star")
        }


        # alternative must be defined
        if(isTRUE(alternative!="both" && alternative!="lower" && alternative!="upper")){
                stop("alternative must be either both, lower or upper")
        }

        # quant_min needs to be numeric
        if(!is.numeric(quant_min)){
                stop("!is.numeric(quant_min)")
        }

        # quant_max needs to be numeric
        if(!is.numeric(quant_max)){
                stop("!is.numeric(quant_max)")
        }

        # n_bisec needs ot be an integer number
        if(!is.numeric(n_bisec)){
                stop("!is.numeric(n_bisec)")
        }

        if(!all(n_bisec == floor(n_bisec))){
                stop("!all(n_bisec == floor(n_bisec))")
        }

        # tolerance needs to bve a number
        if(!is.numeric(tol)){
                stop("!is.numeric(tol)")
        }

        # Tolerance needs to be bigger than 0
        if(!(tol > 0)){
                stop("tol needs to be bigger than 0")
        }

        # Tolerance needs to be small to yield accurate results
        if(tol>0.01){
                warning("The tolerance is higher than 0.01: The bisection resulds might be imprecise.")
        }

        if(!isTRUE(traceplot)){
                if(!(traceplot==FALSE)){
                        stop("traceplote needs to be TRUE or FALSE")
                }
        }

        #----------------------------------------------------------------------

        c_i <- vector()
        runval_i <- vector()

        #----------------------------------------------------------------------

        # Calculate coverages for start points

        cover_quant_min <- coverage_prob(y_star_hat = y_star_hat,
                                         pred_se = pred_se,
                                         q = quant_min,
                                         y_star = y_star,
                                         alternative = alternative)

        cover_quant_max <- coverage_prob(y_star_hat = y_star_hat,
                                         pred_se = pred_se,
                                         q = quant_max,
                                         y_star = y_star,
                                         alternative = alternative)

        #----------------------------------------------------------------------

        # if the coverage is smaller for both quant take quant_min
        if ((cover_quant_min > 1-(alpha+tol))) {

                warning(paste("observed coverage probability for quant_min =",
                              cover_quant_min,
                              "is bigger than 1-alpha+tol =",
                              1-alpha+tol))

                if(traceplot==TRUE){

                        plot(x=quant_min,
                             y=cover_quant_min-(1-alpha),
                             type="p",
                             pch=20,
                             xlab="calibration value",
                             ylab="obs. coverage - nom. coverage",
                             main=paste("f(quant_min) > 1-alpha+tol"),
                             ylim=c(cover_quant_min-(1-alpha)+tol, -tol))
                        abline(a=0, b=0, lty="dashed")
                        abline(a=tol, b=0, col="grey")
                }

                return(quant_min)
        }

        #----------------------------------------------------------------------

        # if the coverage is bigger for both quant take quant_max

        else if ((cover_quant_max < 1-(alpha-tol))) {

                warning(paste("observed coverage probability for quant_max =",
                              cover_quant_max,
                              "is smaller than 1-alpha-tol =",
                              1-alpha-tol))


                if(traceplot==TRUE){

                        plot(x=quant_max,
                             y=cover_quant_max-(1-alpha),
                             type="p", pch=20,
                             xlab="calibration value",
                             ylab="obs. coverage - nom. coverage",
                             main=paste("f(quant_max) < 1-alpha-tol"),
                             ylim=c(cover_quant_max-(1-alpha)-tol, tol))
                        abline(a=0, b=0, lty="dashed")
                        abline(a=-tol, b=0, col="grey")
                }


                return(quant_max)
        }

        #----------------------------------------------------------------------

        # run bisection

        else for (i in 1:n_bisec) {

                # Calculate midpoint
                c <- (quant_min + quant_max) / 2

                runval <- (1-alpha)-coverage_prob(y_star_hat = y_star_hat,
                                                  pred_se = pred_se,
                                                  q = c,
                                                  y_star = y_star,
                                                  alternative = alternative)

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
        warning('Too many iterations, but the quantile of the last step is returned')

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
