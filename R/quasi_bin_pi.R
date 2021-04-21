

#' Prediction intervals for quasi-binomial data
#'
#' quasi_bin_pi calculates bootstrap calibrated prediction intervals for binomial
#' data with constant overdispersion (quasi-binomial)
#'
#' @param histdat a data frame with two columns (success and failure) containing the historical data
#' @param newdat a data frame with two columns (success and failure) containing the future data
#' @param newsize a vector containing the future cluster sizes
#' @param alternative either "both", "upper" or "lower". alternative specifies if a prediction interval or
#' an upper or a lower prediction limit should be computed
#' @param alpha defines the level of confidence (1-alpha)
#' @param nboot number of bootstraps
#' @param lambda_min lower start value for bisection
#' @param lambda_max upper start value for bisection
#' @param traceplot plot for visualization of the bisection process
#' @param n_bisec maximal number of bisection steps
#'
#' @details This function returns bootstrap calibrated prediction intervals
#' \deqn{[l,u]_m = \hat{y}_m \pm q \sqrt{var(\hat{y}_m - y_m)}}
#' with \eqn{\hat{y}}_m as the predicted future number of successes for \eqn{m=1,...,M} future clusters,
#' \eqn{y}_m as the observed future number of successes, \eqn{\sqrt{var(\hat{y}_m - y_m)}}
#' as the prediction error and \eqn{q} as the bootstrap calibrated coefficient that
#' approximates a multivariate normal distribution. Please note that
#' the predicted future number of successes is based on the future cluster size \eqn{n_m}
#' and the success probability estimated from the historical data \eqn{\pi^{hist}}
#' such that \eqn{\hat{y}_m=\pi^{hist} n_m}. Hence, the prediction intervals are
#' different for each of the \eqn{m} future clusters, if their size is not the same.
#'
#' The whole calibration process is based on a bisection algorithm that is similar
#' to the one described in Menssen and Schaarschmidt 2019. If traceplot=TRUE, a graphical
#' overview about the bisection process is given.
#'
#' @return If newdat is specified: A data frame that contains the future data,
#'  the historical mean (hist_mean), the calibrated coefficient (quant_calib),
#'  the prediction error (pred_se), the prediction interval (lower and upper)
#'  and a statement if the prediction interval covers the future observation (cover).
#'
#'  If newsize is specified: A data frame that contains the future cluster sizes (total)
#'  the historical mean (hist_mean), the calibrated coefficient (quant_calib),
#'  the prediction error (pred_se) and the prediction interval (lower and upper).
#'
#'  If alternative is set to "lower": Lower prediction bounds are computed instead
#'  of a prediction interval.
#'
#'  If alternative is set to "upper": Upper prediction bounds are computed instead
#'  of a prediction interval.
#'
#' @export
#'
#' @importFrom stats glm quasibinomial coef
#'
#' @references
#' Menssen M, Schaarschmidt F.: Prediction intervals for overdispersed binomial data
#' with application to historical controls. Statistics in Medicine. 2019;38:2652-2663.
#' https://doi.org/10.1002/sim.8124
#'
#' @examples
#' #' # Historical data
#' qb_dat1
#'
#' # Future data
#' qb_dat2
#'
#' # Prediction interval using qb_dat2 as future data
#' quasi_bin_pi(histdat=qb_dat1, newdat=qb_dat2)
#'
#' # Upper prediction bound for m=3 future observations
#' # that are based on cluster sizes 40, 50, 60 respectively
#' quasi_bin_pi(histdat=qb_dat1, newsize=c(40, 50, 60), alternative="upper")
#'
#'
quasi_bin_pi <- function(histdat,
                         newdat=NULL,
                         newsize=NULL,
                         alternative="both",
                         alpha=0.05,
                         nboot=10000,
                         lambda_min=0.01,
                         lambda_max=10,
                         traceplot=TRUE,
                         n_bisec=30){

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


        histdat$total <- histdat[,1] + histdat[,2]

        n <- nrow(histdat)

        model <- glm(cbind(histdat[,1], histdat[,2]) ~ 1, family=quasibinomial(link="logit"), histdat)

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
                        stop("'(newdat[,2] must contain integer values only")
                }

                m <- nrow(newdat)
                newdat$total <- newdat[,1 ]+ newdat[,2]

        }


        #-----------------------------------------------------------------------
        ### Some historical parameters

        # Historical phi
        hist_phi <- summary(model)$dispersion

        # If historical phi <= 1, adjust it
        if(hist_phi <= 1){

                hist_phi <- 1.001

                warning("historical data is underdispersed (hist_phi <= 1), \n  dispersionparameter was set to 1.001")
        }


        # Historical pi
        hist_pi <- exp(unname(coef(model)))/(1+exp(unname(coef(model))))
        newdat$hist_pi <- hist_pi

        #-----------------------------------------------------------------------
        ### Sampling of future data

        fut_dat_list <- vector(length=nboot, "list")

        for(b in 1:nboot){

                # phi must be smaller than the sample size
                if(hist_phi > min(newdat$total)){
                        hist_phi <- min(newdat$total)-0.1
                }

                # Future observations for calibration
                fut_dat <- rqbinom(n = m, size = newdat$total,
                                   prob = hist_pi, phi = hist_phi)

                # Total number of trials
                fut_dat$total <- fut_dat[,1] + fut_dat[,2]

                # Sampling of the data on which pi and phi is estimated
                bs_pi_se_dat <- rqbinom(n = nrow(histdat), size = histdat$total,
                                        prob = hist_pi, phi = hist_phi)

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
                bs_pi_se_fit <- glm(cbind(bs_pi_se_dat[,1], bs_pi_se_dat[,2])~1,
                                    family=quasibinomial(), bs_pi_se_dat)

                # BS pi
                bs_pi <- exp(unname(coef(bs_pi_se_fit)))/(1+exp(unname(coef(bs_pi_se_fit))))
                fut_dat$bs_pi <- bs_pi

                # BS phi
                bs_phi <- max(1, summary(bs_pi_se_fit)$dispersion)

                # BS n_total
                bs_n_total <- nrow(bs_pi_se_dat)

                # calculation of the prediction se and y_hat
                fut_dat$pred_se <- NA

                for(d in 1:nrow(fut_dat)){

                        bs_n_fut <- fut_dat$total[d]

                        bs_fut_var <- bs_phi *
                                ((bs_n_fut*bs_pi*(1-bs_pi)) +
                                         ((bs_n_fut^2*bs_pi*(1-bs_pi)) *
                                                  (1/bs_n_total)))

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

                fut_cover <- sum(fut_cover_vec)/length(fut_cover_vec)

                return(fut_cover)

        }

        bisection <- function(f, quant_min, quant_max, n, tol = 1e-3) {


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
                y_fut <- n_fut*hist_pi

                # prediction variance var(y_hat-y)
                fut_var <- hist_phi *
                        ((n_fut*hist_pi*(1-hist_pi)) +
                                 ((n_fut^2*hist_pi*(1-hist_pi)) *
                                          (1/bs_n_total)))

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
                                newdat$cover[d] <-  newdat$succ[d] < upper
                        }


                }

        }

        out <- newdat[colSums(!is.na(newdat)) > 0]

        return(out)
}

