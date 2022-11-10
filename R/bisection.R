



bisection <- function(y_star_hat,
                      pred_se,
                      y_star,
                      alternative,
                      quant_min,
                      quant_max,
                      n_bisec,
                      tol,
                      alpha,
                      traceplot=true){

        # Needs several checks!!!!

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











