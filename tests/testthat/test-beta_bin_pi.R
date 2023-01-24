
###  bb_pi() is implecitely covered, because it generates the output of beta_bin_pi()

test_that("check class and output", {

        pred_int <- beta_bin_pi(histdat=bb_dat1,
                                newsize=50,
                                alternative="upper",
                                traceplot = FALSE,
                                nboot = 100)

        # Test classes
        # Test classes
        expect_s3_class(pred_int,
                        class=c("predint", "betaBinomialPI"))


        names_pi <- names(pred_int)

        expect_equal(names_pi,
                     c("prediction",
                       "newsize",
                       "newdat",
                       "histsize",
                       "histdat",
                       "y_star_hat",
                       "pred_se",
                       "alternative",
                       "q",
                       "pi",
                       "rho",
                       "algorithm"))

        # No. of slots of the output list
        expect_equal(length(pred_int), 12)

        # $prediction has to be a data.frame
        expect_true(is.data.frame(pred_int$prediction))

        # histdat has to be a data.frame
        expect_true(is.data.frame(pred_int$histdat))

        # Algorithm ha to be MS22mod by default
        expect_equal(pred_int$algorithm, "MS22mod")

        # Check newdat
        pred_int1 <- beta_bin_pi(histdat=bb_dat1,
                                newdat = bb_dat2,
                                alternative="upper",
                                traceplot = FALSE,
                                nboot = 100)

        # newdat needs to be a data.frame
        expect_true(is.data.frame(pred_int1$newdat))


})


test_that("newdat and newsize must be specified correctly", {

        # newdat and newsize are not specified
        expect_error(beta_bin_pi(histdat=bb_dat1))

        # newdat and newsize are both specified
        expect_error(beta_bin_pi(histdat=bb_dat1,
                                 newdat=bb_dat2,
                                 newsize=50))

        # newdat is not a data frame
        expect_error(beta_bin_pi(histdat=bb_dat1,
                                 newdat=c(1,2,3)))
})

test_that("histdat must be specified correctly", {

        # newdat and newsize are not specified
        expect_error(beta_bin_pi(histdat=c(1,2,3),
                                 newsize=50))

        # number of successes is not allowed to be 0
        expect_error(beta_bin_pi(histdat=data.frame(x=c(0,0), y=c(3,4)),
                                 newsize=50))

        # number of failures is not allowed to be 0
        expect_error(beta_bin_pi(histdat=data.frame(x=c(3,4), y=c(0,0)),
                                 newsize=50))


})


test_that("alternative", {

        expect_error(beta_bin_pi(histdat=bb_dat1,
                                 newsize=50,
                                 alternative="opper"))



})


test_that("new data must be integer", {

        # newsize is not integer
        expect_error(beta_bin_pi(histdat=bb_dat1,
                                 newsize=c(30, 50.3)))

        # newdat[,1] is not integer
        expect_error(beta_bin_pi(histdat=bb_dat1,
                                 newdat=data.frame(x=3.3, y=70)))

        # newdat[,2] is not integer
        expect_error(beta_bin_pi(histdat=bb_dat1,
                                 newdat=data.frame(x=3, y=70.4)))

        # newdat[,c(1, 2)] is not integer
        expect_error(beta_bin_pi(histdat=bb_dat1,
                                 newdat=data.frame(x=3.3, y=70.4)))



})



























