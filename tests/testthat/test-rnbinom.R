test_that("rnbinom", {

        # Output must be a adata.frame
        expect_true(is.data.frame(rnbinom(n=5, lambda=5, kappa=0.13, offset=c(3,3,2,3,2))))

        # n must be specified correctly
        expect_error(rnbinom(n=-5, lambda=5, kappa=0.13, offset=c(3,3,2,3,2)))
        expect_error(rnbinom(n=0, lambda=5, kappa=0.13, offset=c(3,3,2,3,2)))
        expect_error(rnbinom(n="a", lambda=5, kappa=0.13, offset=c(3,3,2,3,2)))

        # lambda
        expect_error(rnbinom(n=5, lambda=-5, kappa=0.13, offset=c(3,3,2,3,2)))
        expect_error(rnbinom(n=5, lambda=0, kappa=0.13, offset=c(3,3,2,3,2)))
        expect_error(rnbinom(n=5, lambda="a", kappa=0.13, offset=c(3,3,2,3,2)))

        # kappa
        expect_error(rnbinom(n=5, lambda=5, kappa=-0.13, offset=c(3,3,2,3,2)))
        expect_error(rnbinom(n=5, lambda=5, kappa=0, offset=c(3,3,2,3,2)))
        expect_error(rnbinom(n=5, lambda=5, kappa="12", offset=c(3,3,2,3,2)))

        # offset
        expect_error(rnbinom(n=5, lambda=5, kappa=0.13, offset=c(-3,3,2,3,2)))
        expect_error(rnbinom(n=5, lambda=5, kappa=0.13, offset=c(0,3,2,3,2)))
        expect_error(rnbinom(n=5, lambda=5, kappa=0.13, offset=c("-33",3,2,3,2)))
})
