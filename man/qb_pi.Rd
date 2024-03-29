% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qb_pi.R
\name{qb_pi}
\alias{qb_pi}
\title{Simple uncalibrated prediction intervals for quasi-binomial data}
\usage{
qb_pi(
  newsize,
  histsize,
  pi,
  phi,
  q = qnorm(1 - 0.05/2),
  alternative = "both",
  newdat = NULL,
  histdat = NULL,
  algorithm = NULL
)
}
\arguments{
\item{newsize}{number of experimental units in the historical clusters.}

\item{histsize}{number of experimental units in the future clusters.}

\item{pi}{binomial proportion}

\item{phi}{dispersion parameter}

\item{q}{quantile used for interval calculation}

\item{alternative}{either "both", "upper" or "lower"
\code{alternative} specifies, if a prediction interval or
an upper or a lower prediction limit should be computed}

\item{newdat}{additional argument to specify the current data set}

\item{histdat}{additional argument to specify the historical data set}

\item{algorithm}{used to define the algorithm for calibration if called via
\code{quasi_bin_pi}. This argument is not of interest for the calculation
of simple uncalibrated intervals}
}
\value{
\code{qb_pi} returns an object of class \code{c("predint", "quasiBinomailPI")}.
}
\description{
\code{qb_pi()} is a helper function that is internally called by  \code{quasi_bin_pi()}. It
calculates simple uncalibrated prediction intervals for binary
data with constant overdispersion (quasi-binomial assumption).
}
\details{
This function returns a simple uncalibrated prediction interval
\deqn{[l,u]_m = n^*_m \hat{\pi} \pm q \sqrt{\hat{\phi} n^*_m \hat{\pi} (1- \hat{\pi}) +
\frac{\hat{\phi} n^{*2}_m \hat{\pi} (1- \hat{\pi})}{\sum_h n_h}}}

with \eqn{n^*_m} as the number of experimental units in the \eqn{m=1, 2, ... , M} future clusters,
\eqn{\hat{\pi}} as the estimate for the binomial proportion obtained from the
historical data, \eqn{\hat{\phi}} as the estimate for the dispersion parameter
and \eqn{n_h} as the number of experimental units per historical cluster. \cr

The direct application of this uncalibrated prediction interval to real life data
is not recommended. Please use the \code{beta_bin_pi()} functions for real life applications. \cr
}
\examples{
qb_pred <- qb_pi(newsize=50, pi=0.3, phi=3, histsize=c(50, 50, 30), q=qnorm(1-0.05/2))
summary(qb_pred)

}
