test_that("tests for errors", {

        # n must be an integer
        expect_error(rbbinom(n=3.3, size=50, prob=0.1, rho=0.001))

        # length(n) must be 1
        expect_error(rbbinom(n=c(3,3), size=50, prob=0.1, rho=0.001))

        # 'size' must contain integer values only
        expect_error(rbbinom(n=3, size=c(50, 50.3, 40), prob=0.1, rho=0.001))

        # prob is not element of [0,1]
        expect_error(rbbinom(n=3, size=c(50, 50, 40), prob=1.3, rho=0.001))
        expect_error(rbbinom(n=3, size=c(50, 50, 40), prob=-1.3, rho=0.001))

        # Rho must be bigger than 0
        expect_error(rbbinom(n=3, size=c(50, 50, 40), prob=0.1, rho=-0.001))

        # size and n do not match
        expect_error(rbbinom(n=2, size=c(50, 50, 40), prob=0.1, rho=0.001))

})


test_that("output must be a data frame with 2 columns", {

        expect_true(is.data.frame(rbbinom(n=3, size=c(50,50,50), prob=0.1, rho=0.001)))

        expect_equal(ncol(rbbinom(n=3, size=c(50,50,50), prob=0.1, rho=0.001)), 2)



})
