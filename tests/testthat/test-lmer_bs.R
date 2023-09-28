test_that("lmer_bs", {

        # lmer model
        fit <- lmer(y_ijk~(1|a) + (1|b) + (1|a:b), data=c2_dat1)

        # Specify fml
        fml <- vector(length=4, "list")

        names(fml) <- c("a:b", "b", "a", "Residual")

        fml[["a:b"]] <- matrix(nrow=6, ncol=2, data=c(1,1,0,0,0,0, 0,0,1,1,1,1))

        fml[["b"]] <- matrix(nrow=6, ncol=1, data=c(1,1,1,1,1,1))

        fml[["a"]] <- matrix(nrow=6, ncol=2, data=c(1,1,0,0,0,0, 0,0,1,1,1,1))

        fml[["Residual"]] <- diag(6)


        # Model must be of class lmerMod
        expect_error(lmer_bs(model=lm(y_ijk~a, c2_dat1),
                             newdat=c2_dat2))

        # Model must be a random effect model
        expect_error(lmer_bs(model=lmer(y_ijk~a + (1|b)+(1|a:b), c2_dat1),
                             newdat=c2_dat2))

        # All random effects must be specified as (1|random_effect)
        expect_error(lmer_bs(model=lmer(y_ijk~(a|b)+(1|a:b), c2_dat1),
                             newdat=c2_dat2))

        # Newdat or futmat_list must be specified
        expect_error(lmer_bs(model=fit))
        expect_error(lmer_bs(model=fit,
                             newdat=c2_dat2,
                             futmat_list = fml))

        # newdat needs to be a data.frame
        expect_error(lmer_bs(model=fit,
                             newdat=1))

        # colnames of historical data and new data must be the same
        c2_dat2_a <- c2_dat2
        colnames(c2_dat2_a) <- c("y",
                                 "a",
                                 "b")

        expect_error(lmer_bs(model=fit,
                             newdat=c2_dat2_a))

        # futmat_list has to be a list
        expect_error(lmer_bs(model=fit,
                             futmat_list = c("a", "b")))

        expect_true(is.list(lmer_bs(model=fit, newdat=c2_dat2, nboot=3)))
})

