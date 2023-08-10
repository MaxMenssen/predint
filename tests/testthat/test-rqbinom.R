test_that("rqbinom", {

        # Output must be a data.frame
        expect_true(is.data.frame(rqbinom(n=10, size=50, prob=0.1, phi=3)))

        # n
        expect_error(rqbinom(n=0, size=50, prob=0.1, phi=3))
        expect_error(rqbinom(n="0", size=50, prob=0.1, phi=3))
        expect_error(rqbinom(n=0.1, size=50, prob=0.1, phi=3))
        expect_error(rqbinom(n=-1, size=50, prob=0.1, phi=3))

        # size
        expect_error(rqbinom(n=5, size=c(30, 50), prob=0.1, phi=3))
        expect_error(rqbinom(n=5, size=c("30", 50, 50, 50, 50), prob=0.1, phi=3))
        expect_error(rqbinom(n=5, size="a", prob=0.1, phi=3))

        # prob
        expect_error(rqbinom(n=10, size=50, prob=-0.1, phi=3))
        expect_warning(rqbinom(n=10, size=50, prob=0, phi=3))
        expect_warning(rqbinom(n=10, size=50, prob=1, phi=3))
        expect_error(rqbinom(n=10, size=50, prob=1.1, phi=3))
        expect_error(rqbinom(n=10, size=50, prob="a", phi=3))

        # phi
        expect_error(rqbinom(n=10, size=50, prob=0.1, phi=-3))
        expect_error(rqbinom(n=10, size=50, prob=0.1, phi=1))
        expect_error(rqbinom(n=10, size=50, prob=0.1, phi=0.1))
        expect_error(rqbinom(n=10, size=50, prob=0.1, phi=50))

})
