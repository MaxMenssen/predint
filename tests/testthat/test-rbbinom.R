test_that("rbbinom", {

        # Output must be a adata.frame
        expect_true(is.data.frame(rbbinom(n=10, size=50, prob=0.1, rho=0.06)))

        # n
        expect_error(rbbinom(n=0, size=50, prob=0.1, rho=0.06))
        expect_error(rbbinom(n="0", size=50, prob=0.1, rho=0.06))
        expect_error(rbbinom(n=0.1, size=50, prob=0.1, rho=0.06))
        expect_error(rbbinom(n=-1, size=50, prob=0.1, rho=0.06))

        # size
        expect_error(rbbinom(n=5, size=c(30, 50), prob=0.1, rho=0.06))
        expect_error(rbbinom(n=5, size=c("30", 50, 50, 50, 50), prob=0.1, rho=0.06))
        expect_error(rbbinom(n=5, size="a", prob=0.1, rho=0.06))

        # prob
        expect_error(rbbinom(n=10, size=50, prob=-0.1, rho=0.06))
        expect_warning(rbbinom(n=10, size=50, prob=0, rho=0.06))
        expect_warning(rbbinom(n=10, size=50, prob=1, rho=0.06))
        expect_error(rbbinom(n=10, size=50, prob=1.1, rho=0.06))
        expect_error(rbbinom(n=10, size=50, prob="a", rho=0.06))

        #rho
        expect_error(rbbinom(n=10, size=50, prob=0.1, rho=-0.06))
        expect_error(rbbinom(n=10, size=50, prob=0.1, rho=0))
        expect_error(rbbinom(n=10, size=50, prob=0.1, rho=1))
})
