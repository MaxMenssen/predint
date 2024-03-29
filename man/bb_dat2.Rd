% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_documentation.R
\docType{data}
\name{bb_dat2}
\alias{bb_dat2}
\title{Beta-binomial data (example 2)}
\format{
A \code{data.frame} with 3 rows and 2 columns:
\describe{
  \item{succ}{number of successes}
  \item{fail}{number of failures}
}
}
\usage{
bb_dat2
}
\description{
This data set contains sampled beta-binomial data from 3 clusters each of
different size. The data set was sampled with \code{rbbinom(n=3, size=c(40, 50, 60), prob=0.1, rho=0.06)}.
}
\keyword{datasets}
