% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quasi_bin_pi.R
\name{quasi_bin_pi}
\alias{quasi_bin_pi}
\title{Prediction intervals for quasi-binomial data}
\usage{
quasi_bin_pi(
  histdat,
  newdat = NULL,
  newsize = NULL,
  alternative = "both",
  alpha = 0.05,
  nboot = 10000,
  delta_min = 0.01,
  delta_max = 10,
  tolerance = 0.001,
  traceplot = TRUE,
  n_bisec = 30,
  algorithm = "MS22mod"
)
}
\arguments{
\item{histdat}{a \code{data.frame} with two columns (success and failures) containing the historical data}

\item{newdat}{a \code{data.frame} with two columns (success and failures) containing the future data}

\item{newsize}{a vector containing the future cluster sizes}

\item{alternative}{either "both", "upper" or "lower". \code{alternative}
specifies if a prediction interval or an upper or a lower prediction limit
should be computed}

\item{alpha}{defines the level of confidence (1-alpha)}

\item{nboot}{number of bootstraps}

\item{delta_min}{lower start value for bisection}

\item{delta_max}{upper start value for bisection}

\item{tolerance}{tolerance for the coverage probability in the bisection}

\item{traceplot}{if \code{TRUE}: Plot for visualization of the bisection process}

\item{n_bisec}{maximal number of bisection steps}

\item{algorithm}{either "MS22" or "MS22mod" (see details)}
}
\value{
\code{quasi_bin_pi} returns an object of class \code{c("predint", "quasiBinomialPI")}
with prediction intervals or limits in the first entry (\code{$prediction}).
}
\description{
\code{quasi_bin_pi()} calculates bootstrap calibrated prediction intervals for binomial
data with constant overdispersion (quasi-binomial assumption).
}
\details{
This function returns bootstrap-calibrated prediction intervals as well as
lower or upper prediction limits.

If \code{algorithm} is set to "MS22", both limits of the prediction interval
are calibrated simultaneously using the algorithm described in Menssen and
Schaarschmidt (2022), section 3.2.4. The calibrated prediction interval is given
as

\deqn{[l,u]_m = n^*_m \hat{\pi} \pm q^{calib} \hat{se}(Y_m - y^*_m)}
 where
\deqn{\hat{se}(Y_m - y^*_m) = \sqrt{\hat{\phi} n^*_m \hat{\pi} (1- \hat{\pi}) +
\frac{\hat{\phi} n^{*2}_m \hat{\pi} (1- \hat{\pi})}{\sum_h n_h}}}

with \eqn{n^*_m} as the number of experimental units in the future clusters,
\eqn{\hat{\pi}} as the estimate for the binomial proportion obtained from the
historical data, \eqn{q^{calib}} as the bootstrap-calibrated coefficient,
 \eqn{\hat{\phi}} as the estimate for the dispersion parameter
and \eqn{n_h} as the number of experimental units per historical cluster. \cr

If \code{algorithm} is set to "MS22mod", both limits of the prediction interval
are calibrated independently from each other. The resulting prediction interval
is given by

\deqn{[l,u] = \Big[n^*_m \hat{\pi} - q^{calib}_l \hat{se}(Y_m - y^*_m), \quad
n^*_m \hat{\pi} + q^{calib}_u \hat{se}(Y_m - y^*_m) \Big]}

Please note, that this modification does not affect the calibration procedure, if only
prediction limits are of interest.
}
\examples{
# Pointwise prediction interval
pred_int <- quasi_bin_pi(histdat=mortality_HCD, newsize=40, nboot=100)
summary(pred_int)

# Pointwise upper prediction limit
pred_u <- quasi_bin_pi(histdat=mortality_HCD, newsize=40, alternative="upper", nboot=100)
summary(pred_u)

# Please note that nboot was set to 100 in order to decrease computing time
# of the example. For a valid analysis set nboot=10000.

}
\references{
Menssen and Schaarschmidt (2019): Prediction intervals for overdispersed binomial
data with application to historical controls. Statistics in Medicine.
\doi{10.1002/sim.8124} \cr
Menssen and Schaarschmidt (2022): Prediction intervals for all of M future
observations based on linear random effects models. Statistica Neerlandica,
 \doi{10.1111/stan.12260}
}
