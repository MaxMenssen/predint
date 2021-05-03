test_that("newdat and newsize must be specified correctly", {

        # newdat and m are not specified
        expect_error(quasi_pois_pi(histdat=data.frame(qp_dat1)))

        # newdat and m are both specified
        expect_error(quasi_pois_pi(histdat=data.frame(qp_dat1),
                                   newdat=data.frame(qp_dat2),
                                   m=5))

        # newdat is not a data frame
        expect_error(quasi_pois_pi(histdat=data.frame(qp_dat1),
                                   newdat=c(1,2,3)))
})

test_that("histdat must be specified correctly", {


        expect_error(quasi_pois_pi(histdat=c(1,2,3),
                                   m=5))
})


test_that("alternative", {

        # alternative is wrong
        expect_error(quasi_pois_pi(histdat=data.frame(qp_dat1),
                                   m=5,
                                   alternative="opper"))

        # Tests if the data frame is correct if alternative is specified correctly
        ncol_upper <- ncol(quasi_pois_pi(histdat=data.frame(qp_dat1),
                                         m=5,
                                         alternative="upper",
                                         traceplot = FALSE,
                                         nboot = 100))

        ncol_lower <- ncol(quasi_pois_pi(histdat=data.frame(qp_dat1),
                                         m=5,
                                         alternative="lower",
                                         traceplot = FALSE,
                                         nboot = 100))

        ncol_both <- ncol(quasi_pois_pi(histdat=data.frame(qp_dat1),
                                        m=5,
                                        alternative="both",
                                        traceplot = FALSE,
                                        nboot = 100))


        expect_equal(ncol_upper, 5)
        expect_equal(ncol_lower, 5)
        expect_equal(ncol_both, 6)


        # Tests if the data frame is correct if alternative is specified correctly
        ncol_upper_nd <- ncol(quasi_pois_pi(histdat=data.frame(qp_dat1),
                                            newdat=data.frame(qp_dat2),
                                            alternative="upper",
                                            traceplot = FALSE,
                                            nboot = 100))


        ncol_lower_nd <- ncol(quasi_pois_pi(histdat=data.frame(qp_dat1),
                                            newdat=data.frame(qp_dat2),
                                            alternative="lower",
                                            traceplot = FALSE,
                                            nboot = 100))

        ncol_both_nd <- ncol(quasi_pois_pi(histdat=data.frame(qp_dat1),
                                           newdat=data.frame(qp_dat2),
                                           alternative="both",
                                           traceplot = FALSE,
                                           nboot = 100))

        expect_equal(ncol_upper_nd, 6)
        expect_equal(ncol_lower_nd, 6)
        expect_equal(ncol_both_nd, 7)
})


test_that("new data must be integer", {

        # m is not integer
        expect_error(quasi_pois_pi(histdat=data.frame(qp_dat1),
                                   m=3.3))

        # newdat[,1] is not integer
        expect_error(quasi_pois_pi(histdat=data.frame(qp_dat1),
                                   newdat=data.frame(x=c(3.3, 4,5))))

})




























