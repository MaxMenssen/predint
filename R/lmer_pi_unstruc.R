
#' Prediction intervals for future observations based on linear random effects models
#'
#' lmer_pi_unstruc calculates a bootstrap calibrated prediction interval for one or more
#' future observation(s) based on linear random effects models as described in section
#' 3.2.4. of Menssen and Schaarschmidt (2021).
#' Please note, that the bootstrap calibration used here does not consider the sampling
#' structure of the future data, since the calibration values are drawn randomly from
#' bootstrap data sets that have the same structure as the historical data.
#'
#'
#' @param model a random effects model of class lmerMod
#' @param newdat a \code{data.frame} with the same column names as the historical data
#' on which the model depends
#' @param m number of future observations
#' @param alternative either "both", "upper" or "lower". \code{alternative} specifies
#' if a prediction interval or an upper or a lower prediction limit should be computed
#' @param alpha defines the level of confidence (1-\code{alpha})
#' @param nboot number of bootstraps
#' @param lambda_min lower start value for bisection
#' @param lambda_max upper start value for bisection
#' @param traceplot plot for visualization of the bisection process
#' @param n_bisec maximal number of bisection steps
#'
#' @details This function returns a bootstrap calibrated prediction interval
#' \deqn{[l,u] = \hat{y} \pm q \sqrt{\hat{var}(\hat{y} - y)}}
#' with \eqn{\hat{y}} as the predicted future observation,
#' \eqn{y} as the observed future observations, \eqn{\sqrt{\hat{var}(\hat{y} - y)}}
#' as the prediction standard error and \eqn{q} as the bootstrap calibrated coefficient that
#' approximates a quantile of the multivariate t-distribution. \cr
#' Please note that this function relies on linear random effects models that are
#' fitted with lmer() from the lme4 package. Random effects have to be specified as
#' \code{(1|random_effect)}.\cr
#'
#'
#' @return If \code{newdat} is specified: A \code{data.frame} that contains the future data,
#'  the historical mean (hist_mean), the calibrated coefficient (quant_calib),
#'  the prediction standard error (pred_se), the prediction interval (lower and upper)
#'  and a statement if the prediction interval covers the future observation (cover).
#'
#'  If \code{m} is specified: A \code{data.frame} that contains the number of future observations (m)
#'  the historical mean (hist_mean), the calibrated coefficient (quant_calib),
#'  the prediction standard error (pred_se) and the prediction interval (lower and upper).
#'
#'  If \code{alternative} is set to "lower": Lower prediction limits are computed instead
#'  of a prediction interval.
#'
#'  If \code{alternative} is set to "upper": Upper prediction limits are computed instead
#'  of a prediction interval.
#'
#'  If \code{traceplot=TRUE}, a graphical overview about the bisection process is given.
#'
#' @references Menssen and Schaarschmidt (2021): Prediction intervals for all of M future
#' observations based on linear random effects models. Statistica Neerlandica,
#'  \url{https://onlinelibrary.wiley.com/doi/10.1111/stan.12260}
#'
#'
#' @export
#'
#' @importFrom graphics abline lines
#' @importFrom lme4 fixef VarCorr bootMer
#' @importFrom stats vcov
#'
#' @examples
#' # loading lme4
#' library(lme4)
#'
#' # Fitting a random effects model based on c2_dat_1
#' fit <- lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1)
#' summary(fit)
#'
#' # Prediction interval using c2_dat2 as future data
#' \donttest{lmer_pi_unsrtuc(model=fit, newdat=c2_dat2, alternative="both", nboot=100)}
#'
#' # Upper prediction limit for m=3 future observations
#' \donttest{lmer_pi_unsrtuc(model=fit, m=3, alternative="upper", nboot=100)}
#'
#' # Please note that nboot was set to 100 in order to decrease computing time
#' # of the example. For a valid analysis set nboot=10000.
#'
lmer_pi_unstruc <- function(model,
                    newdat=NULL,
                    m=NULL,
                    alternative="both",
                    alpha=0.05,
                    nboot=10000,
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
        f <- model@call$formula

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

        ### m
        if(is.null(m) == FALSE){
                # m must be integer
                if(!isTRUE(m == floor(m))){
                        stop("m must be integer")
                }

                if(length(m) > 1){
                        stop("length(m) > 1")
                }


        }


        ### Actual data
        if(is.null(newdat) == FALSE){

                # newdat needs to be a data.frame
                if(is.data.frame(newdat)==FALSE){
                        stop("newdat is not a data.frame")
                }

                # colnames of historical data and new data must be the same
                if(all(colnames(model@frame) == colnames(newdat))==FALSE){
                        stop("columnames of historical data and newdat are not the same")
                }

                # Define m
                m <- nrow(newdat)


        }

        # alternative must be defined
        if(isTRUE(alternative!="both" && alternative!="lower" && alternative!="upper")){
                stop("alternative must be either both, lower or upper")
        }

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

        # # Smallest BS observation
        # bs_y_min <- min(t(boot_obs$t))
        #
        # # Biggest BS observation
        # bs_y_max <- max(t(boot_obs$t))

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

        # Bootstrapped se
        bs_se<- as.list(as.vector(bs_params$bs_se_y_star))

        # Minimum bootstrapped se
        bs_se_min <- min(as.vector(bs_params$bs_se_y_star))

        # Maximum bootstrapped se
        bs_se_max <- max(as.vector(bs_params$bs_se_y_star))

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

                if(alternative=="both"){
                        for(b in 1:nrow(bs_params)){

                                lower <- bs_params$bs_mu[b]-quant*bs_params$bs_se_y_star[b]
                                upper <- bs_params$bs_mu[b]+quant*bs_params$bs_se_y_star[b]

                                pi_list[[b]] <- c("lower"=lower, "upper"=upper, "quant"=quant)

                        }

                        # Check if both lists have the same length
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

                }

                else if(alternative=="lower"){
                        for(b in 1:nrow(bs_params)){

                                lower <- bs_params$bs_mu[b]-quant*bs_params$bs_se_y_star[b]

                                pi_list[[b]] <- c("lower"=lower, "quant"=quant)

                        }

                        # Check if both lists have the same length
                        if(length(pi_list) != length(ystar_list)){
                                stop("length(pi_list) != length(ystar_list)")
                        }

                        # Length of the lists
                        K <- length(pi_list)

                        # Vector for the Coverage
                        cover_vec <- logical(length=K)

                        for(k in 1:K){
                                cover_vec[k] <- pi_list[[k]]["lower"] < ystar_list[[k]][1]
                        }

                        # Coverage probabilities as the mean of the 1/0 vector
                        coverage <- mean(as.integer(cover_vec))

                }

                else if(alternative=="upper"){
                        for(b in 1:nrow(bs_params)){

                                upper <- bs_params$bs_mu[b]+quant*bs_params$bs_se_y_star[b]

                                pi_list[[b]] <- c("upper"=upper, "quant"=quant)

                        }

                        # Check if both lists have the same length
                        if(length(pi_list) != length(ystar_list)){
                                stop("length(pi_list) != length(ystar_list)")
                        }

                        # Length of the lists
                        K <- length(pi_list)

                        # Vector for the Coverage
                        cover_vec <- logical(length=K)

                        for(k in 1:K){
                                cover_vec[k] <- pi_list[[k]]["upper"] > ystar_list[[k]][2]
                        }

                        # Coverage probabilities as the mean of the 1/0 vector
                        coverage <- mean(as.integer(cover_vec))

                }


                return(coverage)

        }


        #----------------------------------------------------------------------
        ### Bisection

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

        # Calculation of the degreees of freedom
        quant_calib <- bisection(f=coverfun, quant_min=lambda_min, quant_max=lambda_max, n=n_bisec)

        #----------------------------------------------------------------------

        # calibrated PI

        lower <- mu_hat-quant_calib*se_y_star_hat
        upper <- mu_hat+quant_calib*se_y_star_hat

        # Define output if newdat is given
        if(is.null(newdat)==FALSE){


                if(alternative=="both"){
                        pi_final <- data.frame("hist_mean"=mu_hat,
                                               "quant_calib"=quant_calib,
                                               "pred_se"=se_y_star_hat,
                                               "lower"=lower,
                                               "upper"=upper)

                        # extract the dependent variable from newdat
                        dep_var <- newdat[,as.character(model@call$formula)[2]]

                        # open vector for coverage
                        cover <- logical(length=nrow(newdat))

                        for(j in 1:nrow(newdat)){
                                cover[j] <- pi_final$lower < dep_var[j] && dep_var[j] < pi_final$upper

                        }

                        cover <- data.frame("cover"=cover)

                        out <- cbind(merge(newdat, pi_final), cover)
                }

                else if(alternative=="lower"){
                        pi_final <- data.frame("hist_mean"=mu_hat,
                                               "quant_calib"=quant_calib,
                                               "pred_se"=se_y_star_hat,
                                               "lower"=lower)

                        # extract the dependent variable from newdat
                        dep_var <- newdat[,as.character(model@call$formula)[2]]

                        # open vector for coverage
                        cover <- logical(length=nrow(newdat))

                        for(j in 1:nrow(newdat)){
                                cover[j] <- pi_final$lower < dep_var[j]

                        }

                        cover <- data.frame("cover"=cover)

                        out <- cbind(merge(newdat, pi_final), cover)
                }

                else if(alternative=="upper"){
                        pi_final <- data.frame("hist_mean"=mu_hat,
                                               "quant_calib"=quant_calib,
                                               "pred_se"=se_y_star_hat,
                                               "upper"=upper)

                        # extract the dependent variable from newdat
                        dep_var <- newdat[,as.character(model@call$formula)[2]]

                        # open vector for coverage
                        cover <- logical(length=nrow(newdat))

                        for(j in 1:nrow(newdat)){
                                cover[j] <- dep_var[j] < pi_final$upper

                        }

                        cover <- data.frame("cover"=cover)

                        out <- cbind(merge(newdat, pi_final), cover)
                }

        }

        # if m is given
        else if(is.null(newdat)){

                if(alternative=="both"){
                        out <- data.frame("m"=m,
                                          "hist_mean"=mu_hat,
                                          "quant_calib"=quant_calib,
                                          "pred_se"=se_y_star_hat,
                                          "lower"=lower,
                                          "upper"=upper)
                }

                if(alternative=="lower"){
                        out <- data.frame("m"=m,
                                          "hist_mean"=mu_hat,
                                          "quant_calib"=quant_calib,
                                          "pred_se"=se_y_star_hat,
                                          "lower"=lower)
                }

                if(alternative=="upper"){
                        out <- data.frame("m"=m,
                                          "hist_mean"=mu_hat,
                                          "quant_calib"=quant_calib,
                                          "pred_se"=se_y_star_hat,
                                          "upper"=upper)
                }

        }



        return(out)

}

