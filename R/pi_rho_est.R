
#' Estimation of the binomial proportion and the intra class correlation.
#'
#' pi_rho_est estimates the overall binomial proportion \eqn{\hat{\pi}} and the intra
#' class correlation \eqn{\hat{\rho}} of data that is assumed to follow the beta-binomial
#' distribution. The estimation of \eqn{\hat{\pi}} and \eqn{\hat{\rho}} is done following
#' the approach of Lui et al. 2000.
#'
#' @param dat a \code{data.frame} with two columns (successes and failures)
#'
#' @return a vector containing estimates for \eqn{\pi} and \eqn{\rho}
#' @export
#'
#' @references
#' Lui, K.-J., Mayer, J.A. and Eckhardt, L: Confidence intervals for the risk ratio
#' under cluster sampling based on the beta-binomial model. Statistics in Medicine.2000;19:2933-2942.
#' \url{https://doi.org/10.1002/1097-0258(20001115)19:21<2933::AID-SIM591>3.0.CO;2-Q}
#'
#'
#' @examples
#' # Estimates for bb_dat1
#' pi_rho_est(bb_dat1)
#'
pi_rho_est <- function(dat){

        if(ncol(dat) != 2){
                stop("ncol(dat)!=2, pi and rho can not be estimated")
        }

        # Calculation of the proportion
        if(all(dat[,1]==0))
        {
                dat[,1][1] <- 0.5
                dat[,2][1] <- dat[,2][1]-0.5

                warning("all(dat[,1]==0), hence the following adjustment was done:
                        dat[,1][1] <- 0.5
                        dat[,2][1] <- dat[,2][1]-0.5")
        }
        #

        if(all(dat[,2]==0))
        {
                dat[,2][1] <- 0.5
                dat[,1][1] <- dat[,1][1]-0.5

                warning("all(dat[,2]==0), hence the following adjustment was done:
                        dat[,2][1] <- 0.5
                        dat[,1][1] <- dat[,1][1]-0.5")
        }
        #

        Yj <- dat[,1]
        mj <- rowSums(dat)
        k <- nrow(dat)

        pi_hat <- sum(Yj)/sum(mj)


        ### Intra class correlation nach Lui et al 2000

        # Between mean squared error
        part1B <- sum(Yj^2/mj)
        part2B <- (sum(Yj)^2/sum(mj))
        part3B <- k-1

        BMS <- (part1B-part2B)/part3B


        # Within mean squared error
        part1W <- sum(Yj)
        part2W <- sum(Yj^2/mj)
        part3W <- sum(mj-1)

        WMS <- (part1W-part2W)/part3W


        # mstar
        part1m <- sum(mj)^2
        part2m <- sum(mj^2)
        part3m <- (k-1)*sum(mj)

        mstar <- (part1m-part2m)/part3m


        # Estimated intrasclass correlation
        rho_hat <- (BMS-WMS)/(BMS+(mstar-1)*WMS)

        out <- c("pi_hat"=pi_hat, "rho_hat"=rho_hat)


        return(out)
}






