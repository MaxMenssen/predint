test_that("plot must be a ggplot", {

        # Quasi-Poisson
        pred_int_qp_pi <- quasi_pois_pi(histdat=qp_dat1,
                                        newdat=qp_dat2,
                                        nboot=1000,
                                        traceplot = FALSE)

        qp_plot <- plot(pred_int_qp_pi)

        # Is the grafic a ggplot object
        expect_true(inherits(qp_plot, c("gg", "ggplot")))

        # Is the format ok?
        # expect_equal(length(qp_plot), 9)
        expect_true(is.list(qp_plot))

})
