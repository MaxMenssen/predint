test_that("model and newdat must be specified correctly", {
        # random effects must be specified as (1|rf)
        expect_error(lmer_pi_futmat(model=lme4::lmer(y_ijk~(b|a), c2_dat1),
                                    newdat=c2_dat2, traceplot=FALSE))

        # newdat is not a data frame
        expect_error(lmer_pi_futmat(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1),
                                    newdat=c(1,2,3), traceplot=FALSE, nboot=100))

        # Newdat needs to be either a data frame or 1
        expect_error(lmer_pi_futmat(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1),
                                    newdat="a", traceplot=FALSE))
})

test_that("futmat_list must be specified correctly", {

        fml <- vector(length=4, "list")

        names(fml) <- c("a:b", "b", "a", "Residual")

        fml[["a:b"]] <- matrix(nrow=6, ncol=2, data=c(1,1,0,0,0, 0, 0,0,1,1,1,1))

        fml[["b"]] <- matrix(nrow=6, ncol=1, data=c(1,1,1,1,1,1))

        fml[["a"]] <- matrix(nrow=6, ncol=2, data=c(1,1,0,0,0,0, 0,0,1,1,1,1))

        # All entries need to be a matrix
        fml[["Residual"]] <- "a"
        expect_error(lmer_pi_futmat(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1),
                                    futmat_list=fml, alternative="both", nboot=100))

        # all entries need to be 1 or 0n
        fml[["Residual"]] <- matrix(nrow=6, ncol=1, data=c(1,1,1,2,3,4))
        expect_error(lmer_pi_futmat(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1),
                                   futmat_list=fml, alternative="both", nboot=100))

        # all entries have to have the same nrow
        fml[["Residual"]] <- matrix(nrow=5, ncol=1, data=c(1,1,1,1,1))
        expect_error(lmer_pi_futmat(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1),
                                    futmat_list=fml, alternative="both", nboot=100))




})




test_that("alternative and output", {

        # alternative
        expect_error(lmer_pi_futmat(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1),
                                    newdat=1,
                                    alternative="opper",
                                    nboot = 100))


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



