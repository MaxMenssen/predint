# predint 1.0.0

* This is the first version of predint


# predint 1.1.0
* lmer_pi() is now deprecated, but three new functions are introduced for the
computation of PI based on random effects models fit by lme4::lmer(). The new 
functions are lmer_pi_unstruc(), lmer_pi_futvec() and lmer_pi_futmat().

# predint 1.1.1
* Argument for the tolerance in the bisection
* Small changes in the readme
* Bugfix in beta_bin_pi
* Changed lambda_min and lambda_max to delta_min and delta_max in order
        to reflect the notation of the manuscript submitted to JSS
* Bugfix in rbbinom: rho must be bigger than 0 but smaller than 1
* Function for the sampling of bs-data based on a random effects model fit with lme4::lmer()
  as used in lmer_pi_futmat()
  
# predint 2.0.0
* the package is now based on s3 objects
* the calibration algorithm works now independently for both limits 
        (default for discrete intervals)
* rqpois() can be used with offsets and returns a data.frame (instead of a
        vector as before)
* quasi_pois_pi() works now with offsets and based on the modified calibration algorithm
* new functions
    - summary.predint 
    - plot.predint 
    - as.data.frame.predint
    - print.predint


# predint 2.0.1

* New real life data 
  - Mortality of mice
  - revertant bacteria colonies

* new functions
  - rnbinom
  - nb_pi
  - neg_bin_pi

plot function needs some bugfixes:  
- lmer_pi_.... plot does not work if endpoint is directly transformed in lmer like lmer(lig(y) ~ ......)
- plot does not work for uncalibrated intervals

negative binomial PI
- documentation for all mb functions
    - done for rnbinom
    -
- write checks
