###  normal_pi() is implecitely covered, because it generates the output of
# the lmer_pi_... functions

test_that("check class and output", {

        fit <- lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1)
        pred_int <- lmer_pi_unstruc(model=fit,
                                   newdat=c2_dat2,
                                   alternative="both",
                                   traceplot=FALSE,
                                   nboot=100)

        # Test classes
        expect_s3_class(pred_int,
                        class=c("predint", "normalPI"))


        names_pi <- names(pred_int)

        expect_equal(names_pi,
                     c("prediction",
                       "newdat",
                       "futmat_list",
                       "futvec",
                       "histdat",
                       "y_star_hat",
                       "pred_se",
                       "alternative",
                       "q",
                       "mu",
                       "pred_var",
                       "m",
                       "algorithm"))

        # No. of slots of the output list
        expect_equal(length(pred_int), 13)

        # $prediction has to be a data.frame
        expect_true(is.data.frame(pred_int$prediction))

        # histdat has to be a data.frame
        expect_true(is.data.frame(pred_int$histdat))

        # Algorithm ha to be MS22 by default
        expect_equal(pred_int$algorithm, "MS22")


})



test_that("model, newdat and m must be specified correctly", {

        # newdat and m are not specified
        expect_error(lmer_pi_unstruc(model=c2_dat1))

        # newdat and m are both specified
        expect_error(lmer_pi_unstruc(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1),
                             newdat=c2_dat2,
                             m=10))

        # newdat is not a data frame
        expect_error(lmer_pi_unstruc(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1),
                             newdat=c(1,2,3)))

        # random effects must be specified as (1|rf)
        expect_error(lmer_pi_unstruc(model=lme4::lmer(y_ijk~(b|a), c2_dat1),
                             newdat=c2_dat2))

        # must be of length 1
        expect_error(lmer_pi_unstruc(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1),
                             m=c(10, 11)))
})


test_that("alternative", {

        # alternative
        expect_error(lmer_pi_unstruc(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1),
                             m=3,
                             alternative="opper"))


})
