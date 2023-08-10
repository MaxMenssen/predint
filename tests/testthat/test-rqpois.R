test_that("rqpois", {

        # Output must be a adata.frame
        expect_true(is.data.frame(rqpois(n=5, lambda=5, phi=3, offset=c(3,3,2,3,2))))

        # n must be specified correctly
        expect_error(rqpois(n=-5, lambda=5, phi=3, offset=c(3,3,2,3,2)))
        expect_error(rqpois(n=0, lambda=5, phi=3, offset=c(3,3,2,3,2)))
        expect_error(rqpois(n="a", lambda=5, phi=3, offset=c(3,3,2,3,2)))

        # lambda
        expect_error(rqpois(n=5, lambda=-5, phi=3, offset=c(3,3,2,3,2)))
        expect_error(rqpois(n=5, lambda=0, phi=3, offset=c(3,3,2,3,2)))
        expect_error(rqpois(n=5, lambda="a", phi=3, offset=c(3,3,2,3,2)))

        # phi
        expect_error(rqpois(n=5, lambda=5, phi=-3, offset=c(3,3,2,3,2)))
        expect_error(rqpois(n=5, lambda=5, phi=1, offset=c(3,3,2,3,2)))
        expect_error(rqpois(n=5, lambda=5, phi=0, offset=c(3,3,2,3,2)))
        expect_error(rqpois(n=5, lambda=5, phi="0", offset=c(3,3,2,3,2)))

        # offset
        expect_error(rqpois(n=5, lambda=5, phi=3, offset=c(-3,3,2,3,2)))
        expect_error(rqpois(n=5, lambda=5, phi=3, offset=c(0,3,2,3,2)))
        expect_error(rqpois(n=5, lambda=5, phi=3, offset=c("-33",3,2,3,2)))
})
