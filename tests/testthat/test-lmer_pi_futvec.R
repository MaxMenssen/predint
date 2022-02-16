test_that("model and newdat must be specified correctly", {
        # random effects must be specified as (1|rf)
        expect_error(lmer_pi_futvec(model=lme4::lmer(y_ijk~(b|a), c2_dat1),
                                    newdat=c2_dat2))

        # newdat is not a data frame
        expect_error(lmer_pi_futvec(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1),
                                    futvec=1,
                                    newdat=c(1,2,3)))
})






test_that("futvec must be specified", {

        # newdat and m are not specified
        expect_error(lmer_pi_futvec(model=fit, futvec=c(1,2,1), alternative="both"))

        expect_error(lmer_pi_futvec(model=fit, futvec=c(1,2,"a"), alternative="both"))

        expect_error(lmer_pi_futvec(model=fit, futvec=c(0,2,3), alternative="both"))

        expect_error(lmer_pi_futvec(model=fit, futvec=c(0.1,2,3), alternative="both", nboot=100))

        expect_error(lmer_pi_futvec(model=fit, futvec=c(1,2,30), alternative="both", nboot=100))

        expect_error(lmer_pi_futvec(model=fit, futvec=c(1,2,3),
                                    newdat = c2_dat2, alternative="upper", nboot=100))

})


test_that("alternative and output", {

        # alternative
        expect_error(lmer_pi_futvec(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1),
                                    futvec=3,
                                    alternative="opper"))


        # Tests if the data frame is correct if alternative is specified correctly (hier gehts weiter!!)
        ncol_upper <- ncol(lmer_pi_futvec(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b),
                                                                         c2_dat1),
                                          futvec=3,
                                          alternative="upper",
                                          traceplot = FALSE,
                                          nboot = 100))

        ncol_lower <- ncol(lmer_pi_futvec(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b),
                                                                          c2_dat1),
                                          futvec=3,
                                          alternative="lower",
                                          traceplot = FALSE,
                                          nboot = 100))

        ncol_both <- ncol(lmer_pi_futvec(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b),
                                                                         c2_dat1),
                                         futvec=3,
                                         alternative="both",
                                         traceplot = FALSE,
                                         nboot = 100))

        expect_equal(ncol_upper, 5)
        expect_equal(ncol_lower, 5)
        expect_equal(ncol_both, 6)


        # newdat is set
        expect_warning(ncol_upper_nd <- ncol(lmer_pi_futvec(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b),
                                                                            c2_dat1),
                                             futvec=c(1, 2, 4, 5, 10, 11, 13, 14),
                                             newdat=c2_dat3,
                                             alternative="upper",
                                             traceplot = FALSE,
                                             nboot = 100)))

        expect_warning(ncol_lower_nd <- ncol(lmer_pi_futvec(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b),
                                                                             c2_dat1),
                                             futvec=c(1, 2, 4, 5, 10, 11, 13, 14),
                                             newdat=c2_dat3,
                                             alternative="lower",
                                             traceplot = FALSE,
                                             nboot = 100)))

        expect_warning(ncol_both_nd <- ncol(lmer_pi_futvec(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b),
                                                                            c2_dat1),
                                            futvec=c(1, 2, 4, 5, 10, 11, 13, 14),
                                            newdat=c2_dat3,
                                            alternative="both",
                                            traceplot = FALSE,
                                            nboot = 100)))

        expect_equal(ncol_upper_nd, 8)
        expect_equal(ncol_lower_nd, 8)
        expect_equal(ncol_both_nd, 9)

})



