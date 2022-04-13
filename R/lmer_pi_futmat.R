
#' Prediction intervals for future observations based on linear random effects models
#'
#' lmer_pi_futmat calculates a bootstrap calibrated prediction interval for one or more
#' future observation(s) based on linear random effects models. With this approach,
#' the sampling structure of the future data is taken into account (see below).
#'
#' @param model a random effects model of class lmerMod
#' @param newdat either 1 or a \code{data.frame} with the same column names as the historical data
#' on which \code{model} depends
#' @param futmat_list a list that contains design matrices for each random factor
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
#' \code{(1|random_effect)}.
#'
#' If \code{newdat} is defined, the bootstrapped future observations used for the calibration
#' process mimic the structure of the data set provided via \code{newdat}. The
#' data sampling is based on a list of design matrices (one for each random factor)
#' that can be obtained if \code{newdat} and the model formula are provided to
#' \code{lme4::lFormula()}. Hence, each random factor that is part of the initial
#' model must have at least two replicates in \code{newdat}. \cr
#' If a random factor in the future data set does not have any replicate, a list
#' that contains design matrices (one for each random factor) can be provided via \code{futmat_list}.
#'
#' This function is an implementation of the PI given in Menssen and Schaarschmidt 2021
#' section 3.2.4 except that the bootstrap calibration values are drawn from
#' bootstrap samples that mimic the future data as described above.
#'
#'
#' @return If \code{newdat} is a  \code{data.frame}: A \code{data.frame} that contains the future data,
#'  the historical mean (hist_mean), the calibrated coefficient (quant_calib),
#'  the prediction standard error (pred_se), the prediction interval (lower and upper)
#'  and a statement if the prediction interval covers the future observation (cover).
#'
#'  If \code{newdat=1}: A \code{data.frame} that contains a statement that m=1,
#'  the historical mean (hist_mean), the calibrated coefficient (quant_calib),
#'  the prediction standard error (pred_se) and the prediction interval (lower and upper).
#'
#'  If \code{futmat_list} is defined: A \code{data.frame} that contains the number of future observations (m),
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
#' @export
#'
#' @importFrom graphics abline lines
#' @importFrom lme4 fixef VarCorr bootMer lFormula
#' @importFrom stats vcov rnorm var
#'
#'
#' @references Menssen, M., Schaarschmidt, F.: Prediction intervals for all of M
#' future observations based on linear random effects models. Statistica Neerlandica.
#' \url{https://doi.org/10.1111/stan.12260}
#'
#' @examples
#'
#' # loading lme4
#' library(lme4)
#'
#' # Fitting a random effects model based on c2_dat_1
#' fit <- lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1)
#' summary(fit)
#'
#' #----------------------------------------------------------------------------
#' ### Using newdat
#'
#' # Prediction interval using c2_dat2 as future data
#' lmer_pi_futmat(model=fit, newdat=c2_dat2, alternative="both", nboot=100)
#'
#' # Upper prediction limit for m=1 future observations
#' lmer_pi_futmat(model=fit, newdat=1, alternative="upper", nboot=100)
#'
#' #----------------------------------------------------------------------------
#'
#' ### Using futmat_list
#'
#' # c2_dat4 has no replication for b. Hence the list of design matrices can not be
#' # generated by lme4::lFormula() and have to be provided by hand via futmat_list.
#'
#' c2_dat4
#'
#' # Build a list containing the design matrices
#'
#' fml <- vector(length=4, "list")
#'
#' names(fml) <- c("a:b", "b", "a", "Residual")
#'
#' fml[["a:b"]] <- matrix(nrow=6, ncol=2, data=c(1,1,0,0,0,0, 0,0,1,1,1,1))
#'
#' fml[["b"]] <- matrix(nrow=6, ncol=1, data=c(1,1,1,1,1,1))
#'
#' fml[["a"]] <- matrix(nrow=6, ncol=2, data=c(1,1,0,0,0,0, 0,0,1,1,1,1))
#'
#' fml[["Residual"]] <- diag(6)
#'
#' fml
#'
#' # Please note, that the design matrix for the interaction term a:b is also
#' # provided even there is no replication for b, since it is believed that
#' # both, the historical and the future data descent from the same data generating
#' # process.
#'
#' # Calculate the PI
#' lmer_pi_futmat(model=fit, futmat_list=fml, alternative="both", nboot=100)
#'
#' #----------------------------------------------------------------------------
#'
#' # Please note that nboot was set to 100 in order to decrease computing time
#' # of the example. For a valid analysis set nboot=10000.
#'
lmer_pi_futmat <- function(model,
                           newdat=NULL,
                           futmat_list=NULL,
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

        #-----------------------------------------------------------------------
        ### Actual data

        if(is.null(newdat) & is.null(futmat_list)){
                stop("newdat and futmat_list are both NULL")
        }

        if(!is.null(newdat) & !is.null(futmat_list)){
                stop("newdat and futmat_list are both defined")
        }

        ### newdat
        if(!is.null(newdat)){
                # newdat needs to be a data.frame or 1
                if(is.data.frame(newdat)==FALSE){

                        if(length(newdat) != 1){
                                stop("newdat has to be a data.frame or equal 1")
                        }

                        else if(newdat != 1){
                                stop("newdat has to be a data.frame or equal 1")
                        }
                }

                # Check conditions if newdat is a data frame
                if(is.data.frame(newdat)){

                        # colnames of historical data and new data must be the same
                        if(all(colnames(model@frame) == colnames(newdat))==FALSE){
                                stop("columnames of historical data and newdat are not the same")
                        }
                }
        }

        ### futmat_list
        else if(!is.list(futmat_list)){
                stop("futmat_list needs to be a list that contains the design matrices for each random effect")
        }

        #-----------------------------------------------------------------------

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

        #----------------------------------------------------------------------
        ### Bootstrapping future observations if newdat is given

        if(!is.null(newdat) & is.null(futmat_list)){

                if(is.data.frame(newdat)==FALSE){

                        # Extracting the observations
                        obs_fun <- function(.){
                                bs_dat <- .@frame[,1]
                        }

                        # Bootstrap for the observations
                        boot_obs <- bootMer(model, obs_fun, nsim = nboot)

                        # Bootstrapped data sets
                        bsdat_list <- as.list(as.data.frame(t(boot_obs$t)))

                        # Take only m random observation per data set
                        ystar_fun <- function(.){
                                y_star <- sample(x=., size=1)

                                y_star_min <- min(y_star)
                                y_star_max <- max(y_star)

                                c("y_star_min"=y_star_min,
                                  "y_star_max"=y_star_max)

                        }

                        # List with future observations (y_star)
                        ystar_list <- lapply(bsdat_list, ystar_fun)

                }

                else{
                        # SD for the random factors
                        sd_hist <- as.data.frame(VarCorr(model))[,c("grp", "sdcor")]

                        # Model Frame for the new data without fitting the data
                        modelframe_list <- lFormula(eval(model), data=newdat)

                        # Intercept (fixed effect)
                        mu <- matrix(unname(modelframe_list$X * fixef(model)))

                        # number of future observations
                        n_fut <- length(modelframe_list$X)

                        # Random effects matrix
                        Zt_list <- modelframe_list$reTrms$Ztlist

                        # Names of Zt_list should be the same as for the SD in sd_hist
                        names(Zt_list) <- gsub("\\s", "", gsub("[1|]", "", names(Zt_list)))

                        # Design Matrix for random effects with residuals
                        Zt_list <- lapply(X=Zt_list, as.matrix)
                        Z_list <- c(lapply(X=Zt_list, FUN=t), Residual=list(diag(1, nrow=n_fut)))

                        # Sampling of B bootstrap samples
                        bsdat_list <- vector(length=nboot, "list")

                        for(b in 1:length(bsdat_list)){

                                # Sampling of random effects
                                u_list <- vector("list",nrow(sd_hist))
                                names(u_list) <- names(Z_list)

                                for(c in 1:length(u_list)){
                                        u_c <- rnorm(n=ncol(Z_list[[c]]), mean=0, sd=sd_hist[c, 2])
                                        u_list[[c]] <- u_c
                                }

                                # Random effects times design matrix
                                ZU_list <- Map(function(x, y)  x%*%y, Z_list, u_list)
                                ZU_list[["mu"]] <- mu

                                # Bootstrap data
                                bsdat_list[[b]] <- rowSums(matrix(unlist(ZU_list), ncol = length(ZU_list)))

                        }

                        # Take only m random observation per data set
                        ystar_fun <- function(.){
                                # y_star <- sample(x=., size=m)

                                y_star_min <- min(.)
                                y_star_max <- max(.)

                                c("y_star_min"=y_star_min,
                                  "y_star_max"=y_star_max)

                        }

                        # List with future observations (y_star)
                        ystar_list <- lapply(bsdat_list, ystar_fun)


                }

        }

        #-----------------------------------------------------------------------

        ### Bootstrapping future observations if futmat_list is given

        if(!is.null(futmat_list) & is.null(newdat)){

                # SD for the random factors
                sd_hist <- as.data.frame(VarCorr(model))[,c("grp", "sdcor")]


                if(length(names(futmat_list)) != length(sd_hist$grp)){
                        stop("length(names(futmat_list)) is not the same as the random effects plus the residuals")
                }

                if(!all(unlist(lapply(X=futmat_list, FUN=is.matrix)))){
                        stop("all elemts of futmat_list have to be a matrix")
                }

                # all matrices should contain only 1 and 0
                ozfun <- function(mat){all(mat %in% c(0, 1))}
                if(!all(unlist(lapply(futmat_list, ozfun)))){
                        stop("all matrices should contain only 1 and 0")
                }

                if(var(unname(unlist(lapply(X=futmat_list, FUN=nrow)))) != 0){
                        stop("all matrices need to have the same numbers of rows")
                }

                if(class(try(futmat_list[sd_hist$grp])) == "try-error"){
                        stop("futmat_list needs to have the same names as the random effects in the model. Maybe you forgot the residuals?")
                }

                else{
                        # Intercept (fixed effect)
                        mu <- matrix(1, nrow(futmat_list[[1]])) * fixef(model)

                        Z_list <- futmat_list

                        # Sampling of B bootstrap samples
                        bsdat_list <- vector(length=nboot, "list")

                        for(b in 1:length(bsdat_list)){

                                # Sampling of random effects
                                u_list <- vector("list",nrow(sd_hist))
                                names(u_list) <- names(Z_list)

                                for(c in 1:length(u_list)){

                                        u_c <- rnorm(n=ncol(Z_list[[c]]), mean=0,
                                                     sd=sd_hist[c, 2])
                                        u_list[[c]] <- u_c
                                }

                                # Random effects times design matrix
                                ZU_list <- Map(function(x, y)  x%*%y, Z_list, u_list)
                                ZU_list[["mu"]] <- mu

                                # Bootstrap data
                                bsdat_list[[b]] <- rowSums(matrix(unlist(ZU_list),
                                                                  ncol = length(ZU_list)))

                        }

                        # Take only m random observation per data set
                        ystar_fun <- function(.){
                                # y_star <- sample(x=., size=m)

                                y_star_min <- min(.)
                                y_star_max <- max(.)

                                c("y_star_min"=y_star_min,
                                  "y_star_max"=y_star_max)

                        }

                        # List with future observations (y_star)
                        ystar_list <- lapply(bsdat_list, ystar_fun)
                }
        }


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

        # Bootstrapped mu
        bs_mu<- as.list(as.vector(bs_params$bs_mu))

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

        # Calculation of the calibrated coefficient
        quant_calib <- bisection(f=coverfun, quant_min=lambda_min, quant_max=lambda_max, n=n_bisec)

        #----------------------------------------------------------------------

        # calibrated PI

        lower <- mu_hat-quant_calib*se_y_star_hat
        upper <- mu_hat+quant_calib*se_y_star_hat


        # Define output if newdat is defined
        if(!is.null(newdat) & is.null(futmat_list)){

                # Define output if newdat is given
                if(is.data.frame(newdat)){


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

                # if newdat==1
                else if(is.data.frame(newdat)==FALSE){

                        if(alternative=="both"){
                                out <- data.frame("m"=1,
                                                  "hist_mean"=mu_hat,
                                                  "quant_calib"=quant_calib,
                                                  "pred_se"=se_y_star_hat,
                                                  "lower"=lower,
                                                  "upper"=upper)
                        }

                        if(alternative=="lower"){
                                out <- data.frame("m"=1,
                                                  "hist_mean"=mu_hat,
                                                  "quant_calib"=quant_calib,
                                                  "pred_se"=se_y_star_hat,
                                                  "lower"=lower)
                        }

                        if(alternative=="upper"){
                                out <- data.frame("m"=1,
                                                  "hist_mean"=mu_hat,
                                                  "quant_calib"=quant_calib,
                                                  "pred_se"=se_y_star_hat,
                                                  "upper"=upper)
                        }

                }

        }


        # Define output if futmat_list is defined
        if(!is.null(futmat_list) & is.null(newdat)){

                if(alternative=="both"){
                        out <- data.frame("m"=nrow(futmat_list[[1]]),
                                          "hist_mean"=mu_hat,
                                          "quant_calib"=quant_calib,
                                          "pred_se"=se_y_star_hat,
                                          "lower"=lower,
                                          "upper"=upper)
                }

                if(alternative=="lower"){
                        out <- data.frame("m"=nrow(futmat_list[[1]]),
                                          "hist_mean"=mu_hat,
                                          "quant_calib"=quant_calib,
                                          "pred_se"=se_y_star_hat,
                                          "lower"=lower)
                }

                if(alternative=="upper"){
                        out <- data.frame("m"=nrow(futmat_list[[1]]),
                                          "hist_mean"=mu_hat,
                                          "quant_calib"=quant_calib,
                                          "pred_se"=se_y_star_hat,
                                          "upper"=upper)
                }

        }


        return(out)

}

