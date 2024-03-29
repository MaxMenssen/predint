



#' Store prediction intervals or limits as a \code{data.frame}
#'
#' Get the prediction intervals or limits of an object of class \code{predint} and
#' save them as a \code{data.frame}.
#'
#' @param x object of class \code{predint}
#' @param ... additional arguments to be passed to \code{base::as.data.frame()}
#'
#' @return This function returns the prediction intervals or limits stored in an
#' object of class \code{"predint"} as a \code{data.frame}
#'
#' @export
#'
#' @examples
#' ### PI for quasi-Poisson data
#' \donttest{pred_int <- quasi_pois_pi(histdat=ames_HCD,
#'                           newoffset=3,
#'                           nboot=100,
#'                           traceplot = FALSE)
#'
#' # Return the prediction intervals as a data.frame
#' as.data.frame(pred_int)}
#'
#' # Please note that nboot was set to 100 in order to decrease computing time
#' # of the example. For a valid analysis set nboot=10000.
as.data.frame.predint <- function(x, ...){

        # input needs to be a predint object
        if(!inherits(x, "predint")){
                stop("x must be of class predint")
        }

        out <- as.data.frame(x$prediction, ...)
        return(out)
}




