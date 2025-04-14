
test_that("class must be correct",{
        expect_s3_class(quasi_pois_pi(histdat=data.frame(qp_dat1),
                                      newoffset=c(1,1,1),
                                      alternative="upper",
                                      nboot=100,
                                      traceplot=FALSE),
                        class=c("predint", "quasiPoissonPI"))

        # names of the output object have to be correct
        names_pi <- names(quasi_pois_pi(histdat=data.frame(qp_dat1),
                                        newoffset=c(1,1,1,1,1),
                                        alternative="upper",
                                        traceplot = FALSE,
                                        nboot = 100))

        expect_equal(names_pi, c("prediction",
                                 "newoffset",
                                 "newdat",
                                 "histoffset",
                                 "histdat",
                                 "y_star_hat",
                                 "pred_se",
                                 "alternative",
                                 "adjust",
                                 "q",
                                 "lambda",
                                 "phi",
                                 "algorithm"))
})


test_that("newdat and newsize must be specified correctly", {

        # newdat and m are not specified
        expect_error(quasi_pois_pi(histdat=data.frame(qp_dat1)))

        # newdat and m are both specified
        expect_error(quasi_pois_pi(histdat=data.frame(qp_dat1),
                                   newdat=data.frame(qp_dat2),
                                   newoffset=c(1,1,1,1,1)))

        # newdat is not a data frame
        expect_error(quasi_pois_pi(histdat=data.frame(qp_dat1),
                                   newdat=c(1,2,3)))
})

test_that("histdat must be specified correctly", {


        expect_error(quasi_pois_pi(histdat=c(1,2,3),
                                   newoffset=c(1,1,1,1,1)))
})


test_that("alternative", {

        # alternative is wrong
        expect_error(quasi_pois_pi(histdat=data.frame(qp_dat1),
                                   newoffset=c(1,1,1,1,1),
                                   alternative="opper"))
})





























