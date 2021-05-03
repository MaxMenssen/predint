test_that("tests for errors", {

        # n must be an integer
        expect_error(rqpois(n=3.3, lambda=1, phi=2))

        # length(n) must be 1
        expect_error(rqpois(n=c(3,3), lambda=1, phi=2))


        # lambda is not element of [0,1]
        expect_error(rqpois(n=3, lambda=0, phi=2))

        # phi must be bigger than 1
        expect_error(rqpois(n=3, lambda=1, phi=1))
})


