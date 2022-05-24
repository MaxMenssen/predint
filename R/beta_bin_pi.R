

#' Prediction intervals for beta-binomial data
#'
#' beta_bin_pi calculates bootstrap calibrated prediction intervals for
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
#' @param lambda_min lower start value for bisection
#' @param lambda_max upper start value for bisection
#' @param tolerance tolerance for the coverage probability in the bisection
#' @param traceplot plot for visualization of the bisection process
#' @param n_bisec maximal number of bisection steps
#'
#' @details This function returns bootstrap calibrated prediction intervals
#' \deqn{[l,u]_m = \hat{y}_m \pm q \sqrt{\hat{var}(\hat{y}_m - y_m)}}
#' with \eqn{\hat{y}_m} as the predicted future number of successes for \eqn{m=1,...,M}
#' future clusters, \eqn{y_m} as the observed future number of successes,
#' \eqn{\sqrt{\hat{var}(\hat{y}_m - y_m)}} as the prediction standard error and \eqn{q}
#' as the bootstrap calibrated coefficient that approximates a quantile from a
#' multivariate normal distribution. \cr
#' Please note that the predicted future number of successes is based on the future
#' cluster size \eqn{n_m} and the success probability estimated from the historical
#' data \eqn{\pi^{hist}} such that \eqn{\hat{y}_m=\pi^{hist} n_m}. Hence, the
#' prediction intervals \eqn{[l,u]_m} are different for each of the \eqn{m} future clusters,
#' if their size is not the same.
#'
#' @return If \code{newdat} is specified: A \code{data.frame} that contains the future data,
#'  the historical proportion (hist_prob),
#'  the calibrated coefficient (quant_calib),
#'  the prediction standard error (pred_se),
#'  the prediction interval (lower and upper)
#'  and a statement if the prediction interval covers the future observation (cover).
#'
#'  If \code{newsize} is specified: A \code{data.frame} that contains the future cluster sizes (total)
#'  the historical proportion (hist_prob),
#'  the calibrated coefficient (quant_calib),
#'  the prediction standard error (pred_se)
#'  and the prediction interval (lower and upper).
#'
#'  If \code{alternative} is set to "lower": Lower prediction bounds are computed instead
#'  of a prediction interval.
#'
#'  If \code{alternative} is set to "upper": Upper prediction bounds are computed instead
#'  of a prediction interval.
#'
#'  If traceplot=TRUE, a graphical overview about the bisection process is given.
#'
#' @export
#'
#' @importFrom graphics abline lines
#'
#' @examples
#' # Historical data
#' bb_dat1
#'
#' # Future data
#' bb_dat2
#'
#' # Prediction interval using bb_dat2 as future data
#' beta_bin_pi(histdat=bb_dat1, newdat=bb_dat2, nboot=100)
#'
#' # Upper prediction bound for m=3 future numbers of success
#' # that are based on cluster sizes 40, 50, 60 respectively
#' beta_bin_pi(histdat=bb_dat1, newsize=c(40, 50, 60), alternative="upper", nboot=100)
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
                        lambda_min=0.01,
                        lambda_max=10,
                        tolerance = 1e-3,
                        traceplot=TRUE,
                        n_bisec=30){

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

        histdat$total <- histdat[,1] + histdat[,2]

        n <- nrow(histdat)
        N_hist <- sum(histdat$total)

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

                total <- newsize
                newdat <- as.data.frame(total)
                m <- nrow(newdat)
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
                        stop("(newdat[,2] must contain integer values only")
                }

                if(nrow(newdat) > nrow(histdat)){
                        warning("The calculation of a PI for more future than historical observations is not recommended")
                }

                m <- nrow(newdat)
                newdat$total <- newdat[,1 ]+ newdat[,2]

        }



        #-----------------------------------------------------------------------
        ### Some historical parameters

        # Historical pi and rho
        pi_rho_hat <- pi_rho_est(histdat[,1:2])

        # Overall pi
        pi_hat <- pi_rho_hat[1]
        newdat$hist_prob <- pi_hat

        # Overall rho
        rho_hat <- pi_rho_hat[2]

        # If rho_hat <= 0, adjust it
        if(rho_hat <= 0){

                rho_hat <- 1e-5
                # newdat$rho_hat <- unname(rho_hat)

                warning("historical data is underdispersed (rho_hat <= 0), \n  rho_hat was set to 0.00001")
        }

        #-----------------------------------------------------------------------
        ### Sampling of bootstrap data

        fut_dat_list <- vector(length=nboot, "list")

        for(b in 1:nboot){

                # Future observations for calibration
                fut_dat <- rbbinom(n = m, size = newdat$total,
                                   prob = pi_hat, rho = rho_hat)

                # Total number of future trials
                fut_dat$total <- fut_dat[,1] + fut_dat[,2]

                # Sampling of historical data on which pi and phi is estimated
                bs_pi_se_dat <- rbbinom(n = n, size = histdat$total,
                                        prob = pi_hat, rho = rho_hat)

                bs_pi_se_dat$total <- bs_pi_se_dat[,1] + bs_pi_se_dat[,2]

                # print("bs_pi_se_dat"); print(bs_pi_se_dat)

                # If all succes are 0 adjust
                if(all(bs_pi_se_dat[,1]==0)){
                        bs_pi_se_dat[1,1] <- bs_pi_se_dat[1,1]+0.5
                        bs_pi_se_dat[1,2] <- bs_pi_se_dat[1,2]-0.5

                }

                # If all failure are 0 adjust
                if(all(bs_pi_se_dat[,2]==0)){
                        bs_pi_se_dat[1,2] <- bs_pi_se_dat[1,2]+0.5
                        bs_pi_se_dat[1,1] <- bs_pi_se_dat[1,1]-0.5

                }

                # bootstrapped parameters
                bs_pi_rho <- pi_rho_est(bs_pi_se_dat[,1:2])

                # BS pi
                bs_pi <- unname(bs_pi_rho[1])
                fut_dat$bs_pi <- bs_pi

                # BS phi
                bs_rho <- unname(max(1e-5, bs_pi_rho[2]))
                fut_dat$bs_rho <- bs_rho

                # Total number of observations per hist. bs-data
                bs_N <- sum(bs_pi_se_dat$total)
                fut_dat$bs_N <- bs_N

                # calculation of the prediction se and y_hat
                fut_dat$pred_se <- NA

                for(d in 1:nrow(fut_dat)){


                        bs_n_fut <- fut_dat$total[d]

                        # Variance for the fut. random variable
                        bs_fut_var_y <-(bs_n_fut*bs_pi*(1-bs_pi))*(1+(bs_n_fut-1)*bs_rho)

                        # Variance for the prediction
                        bs_fut_var_y_hat <- bs_n_fut* (bs_pi*(1-bs_pi))/bs_N +
                                bs_n_fut * bs_pi*(1-bs_pi) * bs_rho * (bs_N-1)/bs_N

                        # bs_fut_var_old <- bs_fut_var_y*(1+bs_n_fut/sum(bs_pi_se_dat$total))

                        bs_fut_var <- bs_fut_var_y + bs_fut_var_y_hat

                        fut_dat$pred_se[d] <- sqrt(bs_fut_var)
                }

                # calculation of y_hat
                fut_dat$y_hat <- fut_dat$total * fut_dat$bs_pi

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

                if(length(fut_cover_vec[which(is.na(fut_cover_vec))]) > 0){

                        stop(fut_dat_list[[which(is.na(fut_cover_vec))]])

                }

                fut_cover <- sum(fut_cover_vec)/length(fut_cover_vec)

                return(fut_cover)

        }

        bisection <- function(f, quant_min, quant_max, n, tol=tolerance) {


                c_i <- vector()
                runval_i <- vector()

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

        # Calculation of the quantile
        quant_calib <- bisection(f=cover_fun,
                                 quant_min=lambda_min,
                                 quant_max=lambda_max,
                                 n=n_bisec)

        newdat$quant_calib <- quant_calib

        newdat$pred_se <- NA

        if(alternative=="both"){
                newdat$lower <- NA
                newdat$upper <- NA
                newdat$cover <- NA
        }

        if(alternative=="lower"){
                newdat$lower <- NA
                newdat$cover <- NA
        }

        if(alternative=="upper"){
                newdat$upper <- NA
                newdat$cover <- NA
        }

        for(d in 1:nrow(newdat)){

                # Number of fut. observations
                n_fut <- newdat$total[d]

                # predicted y
                y_fut <- n_fut*pi_hat

                # variance of y
                bs_fut_var_y <-(n_fut*pi_hat*(1-pi_hat))*(1+(n_fut-1)*rho_hat)

                # variance of y_hat
                bs_fut_var_y_hat <- n_fut * pi_hat * (1-pi_hat) / N_hist +
                        n_fut * pi_hat * (1-pi_hat) * rho_hat * (N_hist-1)/N_hist

                # var(y_hat-y)
                fut_var <- bs_fut_var_y + bs_fut_var_y_hat

                # se(y_hat-y)
                newdat$pred_se[d] <- sqrt(fut_var)

                # Prediction intervals
                if(alternative=="both"){

                        lower <- y_fut - quant_calib*newdat$pred_se[d]
                        upper <- y_fut + quant_calib*newdat$pred_se[d]

                        newdat$lower[d] <- max(0, lower)
                        newdat$upper[d] <- min(newdat$total[d], upper)

                        if(!is.null(newdat) && is.null(newsize)){
                                newdat$cover[d] <- lower < newdat$succ[d] && newdat$succ[d] < upper
                        }



                }

                # Lower prediction bound
                if(alternative=="lower"){

                        lower <- y_fut - quant_calib*newdat$pred_se[d]

                        newdat$lower[d] <- max(0, lower)

                        if(!is.null(newdat) && is.null(newsize)){
                                newdat$cover[d] <- lower < newdat$succ[d]
                        }


                }

                # Upper Prediction bound
                if(alternative=="upper"){
                        upper <- y_fut + quant_calib*newdat$pred_se[d]

                        newdat$upper[d] <- min(newdat$total[d], upper)

                        if(!is.null(newdat) && is.null(newsize)){
                                newdat$cover[d] <- newdat$succ[d] < upper
                        }


                }




        }

        out <- newdat[colSums(!is.na(newdat)) > 0]

        return(out)


}

