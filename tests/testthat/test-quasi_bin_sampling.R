test_that("tests for errors", {

        # n must be an integer
        expect_error(rqbinom(n=3.3, size=50, prob=0.1, phi=2))

        # length(n) must be 1
        expect_error(rqbinom(n=c(3,3), size=50, prob=0.1, phi=2))

        # 'size' must contain integer values only
        expect_error(rqbinom(n=3, size=c(50, 50.3, 40), prob=0.1, phi=2))

        # prob is not element of [0,1]
        expect_error(rqbinom(n=3, size=c(50, 50, 40), prob=1.3, phi=2))
        expect_error(rqbinom(n=3, size=c(50, 50, 40), prob=-1.3, phi=2))

        # Rho must be bigger than 1
        expect_error(rqbinom(n=3, size=c(50, 50, 40), prob=0.1, phi=1))

        # size and n do not match
        expect_error(rqbinom(n=2, size=c(11, 8, 10), prob=0.1, phi=2))

        # phi must be smaller than min(size)
        expect_error(rqbinom(n=2, size=c(8, 10), prob=0.1, phi=8.5))

})


test_that("output must be a data frame with 2 columns", {

        expect_true(is.data.frame(rqbinom(n=3, size=c(50,50,50), prob=0.1, phi=2)))

        expect_equal(ncol(rqbinom(n=3, size=c(50,50,50), prob=0.1, phi=2)), 2)



})
