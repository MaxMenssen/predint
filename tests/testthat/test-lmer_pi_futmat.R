###  normal_pi() is implecitely covered, because it generates the output of
# the lmer_pi_... functions

test_that("check class and output", {

        fit <- lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1)
        pred_int <- lmer_pi_futmat(model=fit,
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

        fit <- lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1)

        fml <- vector(length=4, "list")

        names(fml) <- c("a:b", "b", "a", "Residual")

        fml[["a:b"]] <- matrix(nrow=6, ncol=2, data=c(1,1,0,0,0, 0, 0,0,1,1,1,1))

        fml[["b"]] <- matrix(nrow=6, ncol=1, data=c(1,1,1,1,1,1))

        fml[["a"]] <- matrix(nrow=6, ncol=2, data=c(1,1,0,0,0,0, 0,0,1,1,1,1))

        # All entries need to be a matrix
        fml[["Residual"]] <- "a"

        expect_error(lmer_pi_futmat(model=lfit,
                                    futmat_list=fml,
                                    alternative="both",
                                    nboot=100))

        # all entries need to be 1 or 0n
        fml[["Residual"]] <- matrix(nrow=6, ncol=1, data=c(1,1,1,2,3,4))

        expect_error(lmer_pi_futmat(model=fit,
                                    futmat_list=fml,
                                    alternative="both",
                                    nboot=100))

        # all entries have to have the same nrow
        fml[["Residual"]] <- matrix(nrow=5, ncol=1, data=c(1,1,1,1,1))

        expect_error(lmer_pi_futmat(model=fit,
                                    futmat_list=fml,
                                    alternative="both",
                                    nboot=100))


        # Is futmat_list correct in the output
        fml[["Residual"]] <- diag(6)
        pred_int <- lmer_pi_futmat(model=fit,
                                   futmat_list=fml,
                                   alternative="both",
                                   traceplot=FALSE,
                                   nboot=100)

        expect_true(is.list(pred_int$futmat_list))
        expect_equal(length(pred_int$futmat_list), 4)


})




test_that("alternative", {

        # alternative
        expect_error(lmer_pi_futmat(model=lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1),
                                    newdat=1,
                                    alternative="opper",
                                    nboot = 100))


})



