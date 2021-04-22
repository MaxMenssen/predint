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

        # newdat and newsize are not specified
        expect_error(beta_bin_pi(histdat=bb_dat1,
                                 newsize=50,
                                 alternative="opper"))

        # Tests if the data frame is correct if alternative is specified correctly
        ncol_upper <- ncol(beta_bin_pi(histdat=bb_dat1,
                                       newsize=50,
                                       alternative="upper",
                                       traceplot = FALSE,
                                       nboot = 100))

        ncol_lower <- ncol(beta_bin_pi(histdat=bb_dat1,
                                       newsize=50,
                                       alternative="lower",
                                       traceplot = FALSE,
                                       nboot = 100))

        ncol_both <- ncol(beta_bin_pi(histdat=bb_dat1,
                                      newsize=50,
                                      alternative="both",
                                      traceplot = FALSE,
                                      nboot = 100))

        expect_equal(ncol_upper, 4)
        expect_equal(ncol_lower, 4)
        expect_equal(ncol_both, 5)

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



























