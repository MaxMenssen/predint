



#' Store prediction intervals or limits as a \code{data.frame}
#'
#' Get the prediction intervals or limits of an object of class \code{predint} and
#' save them as a \code{data.frame}.
#'
#' @param x object of class \code{predint}
#'
#' @return This function returns the prediction intervals or limits stored in an
#' object of class \code{"predint"} as a \code{data.frame}
#'
#' @export
#'
#' @examples
#' ### PI for quasi-Poisson data
#' pred_int <- quasi_pois_pi(histdat=data.frame(qp_dat1),
#'                           newoffset=c(1,2,3),
#'                           nboot=100,
#'                           traceplot = FALSE)
#'
#' # Return the prediction intervals as a data.frame
#' as.data.frame(pred_int)
as.data.frame.predint <- function(x){

        # input needs to be a predint object
        if(!inherits(x, "predint")){
                stop("x must be of class predint")
        }

        return(pred_int$prediction)
}


