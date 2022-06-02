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
        to reflect the notation of the manuscript
* Bugfix in rbbinom: rho must be bigger than 0 but smaller than 1
* Function for the sampling of bs-data based on a random effects model fit with lme4::lmer()
  as used in lmer_pi_futmat()

TO DO
- Include NTP example data from both papers
- Include bs function used in lmer_pi_futmat as a single function in the package (done)
        - Do a bug search
        - Write tests for lmer_bs()
- Quasi-poisson with offsets
        - rqpois
        - quasi_pois_pi
