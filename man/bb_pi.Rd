% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bb_pi.R
\name{bb_pi}
\alias{bb_pi}
\title{Simple uncalibrated prediction intervals for beta-binomial data}
\usage{
bb_pi(
  newsize,
  histsize,
  pi,
  rho,
  q = qnorm(1 - 0.05/2),
  alternative = "both",
  newdat = NULL,
  histdat = NULL,
  algorithm = NULL
)
}
\arguments{
\item{newsize}{number of experimental units in the historical clusters}

\item{histsize}{number of experimental units in the future clusters}

\item{pi}{binomial proportion}

\item{rho}{intra class correlation}

\item{q}{quantile used for interval calculation}

\item{alternative}{either "both", "upper" or "lower"
\code{alternative} specifies, if a prediction interval or
an upper or a lower prediction limit should be computed}

\item{newdat}{additional argument to specify the current data set}

\item{histdat}{additional argument to specify the historical data set}

\item{algorithm}{used to define the algorithm for calibration if called via
\code{beta_bin_pi()}. This argument is not of interest for the calculation
of simple uncalibrated intervals}
}
\value{
\code{bb_pi()} returns an object of class \code{c("predint", "betaBinomialPI")}
with prediction intervals or limits in the first entry (\code{$prediction}).
}
\description{
\code{bb_pi()} is a helper function that is internally called by \code{beta_bin_pi()}. It
calculates simple uncalibrated prediction intervals for binary
data with overdispersion changing between the clusters (beta-binomial).
}
\details{
This function returns a simple uncalibrated prediction interval
\deqn{[l,u]_m = n^*_m \hat{\pi} \pm q \sqrt{n^*_m \hat{\pi} (1- \hat{\pi}) [1 + (n^*_m -1) \hat{\rho}] +
[\frac{n^{*2}_m \hat{\pi} (1- \hat{\pi})}{\sum_h n_h} + \frac{\sum_h n_h -1}{\sum_h n_h} n^{*2}_m \hat{\pi} (1- \hat{\pi}) \hat{\rho}]}}

with \eqn{n^*_m} as the number of experimental units in the \eqn{m=1, 2, ... , M} future clusters,
\eqn{\hat{\pi}} as the estimate for the binomial proportion obtained from the
historical data, \eqn{\hat{\rho}} as the estimate for the intra class correlation
and \eqn{n_h} as the number of experimental units per historical cluster. \cr

The direct application of this uncalibrated prediction interval to real life data
is not recommended. Please use \code{beta_bin_pi()} for real life applications.  \cr
}
\examples{
# Pointwise uncalibrated PI
bb_pred <- bb_pi(newsize=c(50), pi=0.3, rho=0.05, histsize=rep(50, 20), q=qnorm(1-0.05/2))
summary(bb_pred)

}
