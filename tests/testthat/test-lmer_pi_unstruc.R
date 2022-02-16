test_that("model, newdat and m must be specified correctly", {

        # newdat and m are not specified
        expect_error(lmer_pi_unsrtuc(model=c2_dat1))

        # newdat and m are both specified
        expect_error(lmer_pi_unsrtuc(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1),
                             newdat=c2_dat2,
                             m=10))

        # newdat is not a data frame
        expect_error(lmer_pi_unsrtuc(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1),
                             newdat=c(1,2,3)))

        # random effects must be specified as (1|rf)
        expect_error(lmer_pi_unsrtuc(model=lme4::lmer(y_ijk~(b|a), c2_dat1),
                             newdat=c2_dat2))

        # must be of length 1
        expect_error(lmer_pi_unsrtuc(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1),
                             m=c(10, 11)))
})


test_that("alternative and output", {

        # alternative
        expect_error(lmer_pi_unsrtuc(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1),
                             m=3,
                             alternative="opper"))


        # Tests if the data frame is correct if alternative is specified correctly

        # m is set
        ncol_upper <- ncol(lmer_pi_unsrtuc(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1),
                                   m=3,
                                   alternative="upper",
                                   traceplot = FALSE,
                                   nboot = 100))

        ncol_lower <- ncol(lmer_pi_unsrtuc(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1),
                                   m=3,
                                   alternative="lower",
                                   traceplot = FALSE,
                                   nboot = 100))

        ncol_both <- ncol(lmer_pi_unsrtuc(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1),
                                  m=3,
                                  alternative="both",
                                  traceplot = FALSE,
                                  nboot = 100))

        expect_equal(ncol_upper, 5)
        expect_equal(ncol_lower, 5)
        expect_equal(ncol_both, 6)


        # newdat is set
        ncol_upper_nd <- ncol(lmer_pi_unsrtuc(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1),
                                      newdat=c2_dat2,
                                      alternative="upper",
                                      traceplot = FALSE,
                                      nboot = 100))

        ncol_lower_nd <- ncol(lmer_pi_unsrtuc(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1),
                                      newdat=c2_dat2,
                                      alternative="lower",
                                      traceplot = FALSE,
                                      nboot = 100))

        ncol_both_nd <- ncol(lmer_pi_unsrtuc(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1),
                                     newdat=c2_dat2,
                                     alternative="both",
                                     traceplot = FALSE,
                                     nboot = 100))

        expect_equal(ncol_upper_nd, 8)
        expect_equal(ncol_lower_nd, 8)
        expect_equal(ncol_both_nd, 9)

})
