test_that("test summary() for normalPI", {


        # fit the model
        fit <- lme4::lmer(y_ijk~(1|a)+(1|b)+(1|a:b), c2_dat1)


        #-----------------------------------------------------------------------
        # Calculate the PI using c2_dat2 as newdat
        pred_int_npi <- lmer_pi_futmat(model=fit,
                                   newdat=c2_dat2,
                                   alternative="both",
                                   nboot=100,
                                   traceplot=FALSE)
        # Save the summary
        sum_futmat <- summary(pred_int_npi)

        # data.frame
        expect_true(is.data.frame(sum_futmat))

        # ncol ok?
        expect_equal(ncol(sum_futmat), 9)

        # nrow ok?
        expect_equal(nrow(sum_futmat), 21)

        # variable names ok?
        expect_equal(names(sum_futmat),
                     c("y_ijk",
                       "a",
                       "b",
                       "lower",
                       "upper",
                       "y_star_hat",
                       "q",
                       "pred_se",
                       "cover" ))

        # change row names
        sum_futmat_npi_1 <- summary(pred_int,
                                row.names=21:1)

        expect_true(all(rownames(sum_futmat_npi_1) == 21:1))

        # Check if the print works fine
        expect_output(summary(pred_int_npi),
                      "Simultanious 95 % prediction interval for 21 future observations")

        expect_output(summary(pred_int_npi),
                      "All future observations are covered")

        expect_output(summary(pred_int_npi),
                      "Bootstrap calibration was done following Menssen and Schaarschmidt 2022")

        #-----------------------------------------------------------------------
        # Lower bounds with newdat
        pred_int_nl <- lmer_pi_futmat(model=fit,
                                      newdat=c2_dat2,
                                      alternative="lower",
                                      nboot=100,
                                      traceplot=FALSE)

        # Sumary
        sum_int_nl <- summary(pred_int_nl)

        # data.frame
        expect_true(is.data.frame(sum_int_nl))

        # ncol ok?
        expect_equal(ncol(sum_int_nl), 8)

        # nrow ok?
        expect_equal(nrow(sum_int_nl), 21)

        # variable names ok?
        expect_equal(names(sum_int_nl),
                     c("y_ijk",
                       "a",
                       "b",
                       "lower",
                       # "upper",
                       "y_star_hat",
                       "q",
                       "pred_se",
                       "cover" ))

        # Check if the print works fine
        expect_output(summary(pred_int_nl),
                      "One-sided simultanious 95 % lower prediction limit for 21 future observations")

        expect_output(summary(pred_int_nl),
                      "All future observations are covered")

        expect_output(summary(pred_int_nl),
                      "Bootstrap calibration was done following Menssen and Schaarschmidt 2022")

        #-----------------------------------------------------------------------
        # Lower bounds with newdat
        pred_int_ul <- lmer_pi_futmat(model=fit,
                                      newdat=c2_dat2,
                                      alternative="upper",
                                      nboot=100,
                                      traceplot=FALSE)

        # Sumary
        sum_int_ul <- summary(pred_int_ul)

        # data.frame
        expect_true(is.data.frame(sum_int_ul))

        # ncol ok?
        expect_equal(ncol(sum_int_ul), 8)

        # nrow ok?
        expect_equal(nrow(sum_int_ul), 21)

        # variable names ok?
        expect_equal(names(sum_int_ul),
                     c("y_ijk",
                       "a",
                       "b",
                       # "lower",
                       "upper",
                       "y_star_hat",
                       "q",
                       "pred_se",
                       "cover" ))

        # Check if the print works fine
        expect_output(summary(pred_int_ul),
                      "One-sided simultanious 95 % upper prediction limit for 21 future observations")

        expect_output(summary(pred_int_ul),
                      "All future observations are covered")

        expect_output(summary(pred_int_ul),
                      "Bootstrap calibration was done following Menssen and Schaarschmidt 2022")


        #-----------------------------------------------------------------------
        # PI for m=1

        pred_int_pi1 <- lmer_pi_futmat(model=fit,
                                       newdat=1,
                                       alternative="both",
                                       nboot=100,
                                       traceplot=FALSE)

        # Save the summary
        sum_futmat_pi1 <- summary(pred_int_pi1)

        # data.frame
        expect_true(is.data.frame(sum_futmat_pi1))

        # ncol ok?
        expect_equal(ncol(sum_futmat_pi1), 5)

        # nrow ok?
        expect_equal(nrow(sum_futmat_pi1), 1)

        # variable names ok?
        expect_equal(names(sum_futmat_pi1),
                     c("lower",
                       "upper",
                       "y_star_hat",
                       "q",
                       "pred_se"))

        # Check if the print works fine
        expect_output(summary(pred_int_pi1),
                      "Pointwise 95 % prediction interval for one future observation")

        expect_output(summary(pred_int_pi1),
                      "Bootstrap calibration was done following Menssen and Schaarschmidt 2022")

        #-----------------------------------------------------------------------
        # lower bound for m=1

        pred_int_l1 <- lmer_pi_futmat(model=fit,
                                      newdat=1,
                                      alternative="lower",
                                      nboot=100,
                                      traceplot=FALSE)

        # Save the summary
        sum_futmat_pi1 <- summary(pred_int_l1)

        # data.frame
        expect_true(is.data.frame(sum_futmat_pi1))

        # ncol ok?
        expect_equal(ncol(sum_futmat_pi1), 4)

        # nrow ok?
        expect_equal(nrow(sum_futmat_pi1), 1)

        # variable names ok?
        expect_equal(names(sum_futmat_pi1),
                     c("lower",
                       "y_star_hat",
                       "q",
                       "pred_se"))

        # Check if the print works fine
        expect_output(summary(pred_int_l1),
                      "One-sided pointwise 95 % lower prediction limit for one future observation")

        expect_output(summary(pred_int_l1),
                      "Bootstrap calibration was done following Menssen and Schaarschmidt 2022")

        #-----------------------------------------------------------------------
        # upper bound for m=1

        pred_int_u1 <- lmer_pi_futmat(model=fit,
                                      newdat=1,
                                      alternative="upper",
                                      nboot=100,
                                      traceplot=FALSE)

        # Save the summary
        sum_futmat_pi1 <- summary(pred_int_u1)

        # data.frame
        expect_true(is.data.frame(sum_futmat_pi1))

        # ncol ok?
        expect_equal(ncol(sum_futmat_pi1), 4)

        # nrow ok?
        expect_equal(nrow(sum_futmat_pi1), 1)

        # variable names ok?
        expect_equal(names(sum_futmat_pi1),
                     c("upper",
                       "y_star_hat",
                       "q",
                       "pred_se"))

        # Check if the print works fine
        expect_output(summary(pred_int_u1),
                      "One-sided pointwise 95 % upper prediction limit for one future observation")

        expect_output(summary(pred_int_u1),
                      "Bootstrap calibration was done following Menssen and Schaarschmidt 2022")

        #-----------------------------------------------------------------------

        fml <- vector(length=4, "list")

        names(fml) <- c("a:b", "b", "a", "Residual")

        fml[["a:b"]] <- matrix(nrow=6, ncol=2, data=c(1,1,0,0,0,0, 0,0,1,1,1,1))

        fml[["b"]] <- matrix(nrow=6, ncol=1, data=c(1,1,1,1,1,1))

        fml[["a"]] <- matrix(nrow=6, ncol=2, data=c(1,1,0,0,0,0, 0,0,1,1,1,1))

        fml[["Residual"]] <- diag(6)

        fml

        pred_int_fml_pi <- lmer_pi_futmat(model=fit,
                                          futmat_list=fml,
                                          alternative="both",
                                          nboot=100,
                                          traceplot = FALSE)

        sum_futmat_fml_pi <- summary(pred_int_fml_pi)

        # data.frame
        expect_true(is.data.frame(sum_futmat_fml_pi))

        # ncol ok?
        expect_equal(ncol(sum_futmat_fml_pi), 5)

        # nrow ok?
        expect_equal(nrow(sum_futmat_fml_pi), 6)

        # variable names ok?
        expect_equal(names(sum_futmat_fml_pi),
                     c("lower",
                       "upper",
                       "y_star_hat",
                       "q",
                       "pred_se" ))


        # Check if the print works fine
        expect_output(summary(pred_int_fml_pi),
                      "Simultanious 95 % prediction interval for 6 future observations")

        expect_output(summary(pred_int_fml_pi),
                      "Bootstrap calibration was done following Menssen and Schaarschmidt 2022")
})


test_that("test summary() for binomial PI)", {

        # Calculate the PI using c2_dat2 as newdat
        pred_int_bb_pi <- beta_bin_pi(histdat=bb_dat1,
                                      newdat=bb_dat2,
                                      nboot=1000,
                                      traceplot = FALSE)

        # Save the summary
        sum_futmat_bb_pi <- summary(pred_int_bb_pi)

        # data.frame
        expect_true(is.data.frame(sum_futmat_bb_pi))

        # ncol ok?
        expect_equal(ncol(sum_futmat_bb_pi), 10)

        # nrow ok?
        expect_equal(nrow(sum_futmat_bb_pi), 3)

        # variable names ok?
        expect_equal(names(sum_futmat_bb_pi),
                     c("succ",
                       "fail",
                       "newsize",
                       "lower",
                       "upper",
                       "y_star_hat",
                       "ql",
                       "qu",
                       "pred_se",
                       "cover"))

        # Check if the print works fine
        expect_output(summary(pred_int_bb_pi),
                      "Simultanious 95 % prediction intervals for 3 future observations")

        expect_output(summary(pred_int_bb_pi),
                      "All future observations are covered")

        expect_output(summary(pred_int_bb_pi),
                      "modiefied version of Menssen and Schaarschmidt 2022")

        #-----------------------------------------------------------------------

        # Calculate the PI using c2_dat2 as newdat
        pred_int_bb_pi_30 <- beta_bin_pi(histdat=bb_dat1,
                                         newsize=30,
                                         nboot=1000,
                                         traceplot = FALSE)

        # Save the summary
        sum_futmat_bb_pi_30 <- summary(pred_int_bb_pi_30)

        # data.frame
        expect_true(is.data.frame(sum_futmat_bb_pi_30))

        # ncol ok?
        expect_equal(ncol(sum_futmat_bb_pi_30), 7)

        # nrow ok?
        expect_equal(nrow(sum_futmat_bb_pi_30), 1)

        # variable names ok?
        expect_equal(names(sum_futmat_bb_pi_30),
                     c("lower",
                       "upper",
                       "newsize",
                       "y_star_hat",
                       "ql",
                       "qu",
                       "pred_se"))

        # Check if the print works fine
        expect_output(summary(pred_int_bb_pi_30),
                      "Pointwise 95 % prediction interval for one future observation")

        expect_output(summary(pred_int_bb_pi_30),
                      "modiefied version of Menssen and Schaarschmidt 2022")

})


test_that("test summary() for Poisson PI)", {

        # Calculate the PI using c2_dat2 as newdat
        pred_int_qp_pi <- quasi_pois_pi(histdat=qp_dat1,
                                        newdat=qp_dat2,
                                        nboot=1000,
                                        traceplot = FALSE)

        # Save the summary
        sum_futmat_qp_pi <- summary(pred_int_qp_pi)

        # data.frame
        expect_true(is.data.frame(sum_futmat_qp_pi))

        # ncol ok?
        expect_equal(ncol(sum_futmat_qp_pi), 9)

        # nrow ok?
        expect_equal(nrow(sum_futmat_qp_pi), 3)

        # variable names ok?
        expect_equal(names(sum_futmat_qp_pi),
                     c("y",
                       "offset",
                       "lower",
                       "upper",
                       "y_star_hat",
                       "ql",
                       "qu",
                       "pred_se",
                       "cover"))

        # Check if the print works fine
        expect_output(summary(pred_int_qp_pi),
                      "Simultanious 95 % prediction intervals for 3 future observations")

        expect_output(summary(pred_int_qp_pi),
                      "All future observations are covered")

        expect_output(summary(pred_int_qp_pi),
                      "modiefied version of Menssen and Schaarschmidt 2022")

        #-----------------------------------------------------------------------

        # Calculate the PI using c2_dat2 as newdat
        pred_int_qp_pi_3 <- quasi_pois_pi(histdat=qp_dat1,
                                           newoffset=3,
                                           nboot=1000,
                                           traceplot = FALSE)

        # Save the summary
        sum_futmat_qp_pi_3 <- summary(pred_int_qp_pi_3)

        # data.frame
        expect_true(is.data.frame(sum_futmat_bb_pi_30))

        # ncol ok?
        expect_equal(ncol(sum_futmat_bb_pi_30), 7)

        # nrow ok?
        expect_equal(nrow(sum_futmat_bb_pi_30), 1)

        # variable names ok?
        expect_equal(names(sum_futmat_bb_pi_30),
                     c("lower",
                       "upper",
                       "newsize",
                       "y_star_hat",
                       "ql",
                       "qu",
                       "pred_se"))

        # Check if the print works fine
        expect_output(summary(pred_int_bb_pi_30),
                      "Pointwise 95 % prediction interval for one future observation")

        expect_output(summary(pred_int_bb_pi_30),
                      "modiefied version of Menssen and Schaarschmidt 2022")

})


