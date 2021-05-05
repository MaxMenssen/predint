
<!-- README.md is generated from README.Rmd. Please edit that file -->

# predint

<!-- badges: start -->

<!-- badges: end -->

The package predint provides functions to calculate prediction intervals
for one or more future observations based on overdispersed binomial
data, overdispersed poisson data, as well as based on linear random
effects models fitted with lme4::lmer().

## Installation

You can install the released version of predint from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("predint")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("MaxMenssen/predint")
```

## Example

The use of prediction intervals is heavyly discussed for various
pharmaceutical and biomedical applications such as assessment of
historical control data or the detection of anti-drug antibodies. \\ The
following example focusses on the use of historical control data in
order to varify the outcome of an actual study (bio assay). For this
purpose we use the real data example of Menssen and Schaarschmidt
(2019).

``` r
# load predint
library(predint)

# data set
dat_real <- data.frame("dead"=c(15, 10, 12, 12, 13, 11, 19, 11, 14, 21),
                       "alive"=c(35, 40, 38, 38, 37, 39, 31, 39, 36, 29))

# PI for one future control group comprised of 50 mice
quasi_bin_pi(histdat=dat_real, newsize=50, traceplot = FALSE)
#>   total hist_prob quant_calib  pred_se    lower   upper
#> 1    50     0.276   0.8490039 8.854377 6.282599 21.3174
```
