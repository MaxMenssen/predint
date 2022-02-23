test_that("model and newdat must be specified correctly", {
        # random effects must be specified as (1|rf)
        expect_error(lmer_pi_futmat(model=lme4::lmer(y_ijk~(b|a), c2_dat1),
                                    newdat=c2_dat2))

        # newdat is not a data frame
        expect_error(lmer_pi_futmat(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1),
                                    newdat=c(1,2,3)))

        # Newdat needs to be either a data frame or 1
        expect_error(lmer_pi_futmat(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1),
                                    newdat="a"))
})




test_that("alternative and output", {

        # alternative
        expect_error(lmer_pi_futmat(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1),
                                    newdat=1,
                                    alternative="opper"))


        # Tests if the data frame is correct if alternative is specified correctly (hier gehts weiter!!)
        ncol_upper <- ncol(lmer_pi_futmat(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b),
                                                           c2_dat1),
                                          newdat=1,
                                          alternative="upper",
                                          traceplot = FALSE,
                                          nboot = 100))

        ncol_lower <- ncol(lmer_pi_futmat(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b),
                                                           c2_dat1),
                                          newdat=1,
                                          alternative="lower",
                                          traceplot = FALSE,
                                          nboot = 100))

        ncol_both <- ncol(lmer_pi_futmat(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b),
                                                          c2_dat1),
                                         newdat=1,
                                         alternative="both",
                                         traceplot = FALSE,
                                         nboot = 100))

        expect_equal(ncol_upper, 5)
        expect_equal(ncol_lower, 5)
        expect_equal(ncol_both, 6)


})



