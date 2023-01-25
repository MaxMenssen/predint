test_that("as.data.frame.predint must return a data frame", {

        pred_int_qp_pi <- quasi_pois_pi(histdat=qp_dat1,
                                        newdat=qp_dat2,
                                        nboot=1000,
                                        traceplot = FALSE)

        expect_true(is.data.frame(as.data.frame(pred_int_qp_pi)))
})
