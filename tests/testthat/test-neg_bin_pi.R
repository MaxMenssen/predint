test_that("neg_bin_pi", {

        expect_s3_class(neg_bin_pi(histdat=data.frame(qp_dat1),
                                   newoffset=c(1,1,1),
                                   alternative="upper",
                                   nboot=100,
                                   traceplot=FALSE),
                        class=c("predint", "negativeBinomialPI"))

        # names of the output object have to be correct
        names_pi <- names(neg_bin_pi(histdat=data.frame(qp_dat1),
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
                                 "q",
                                 "lambda",
                                 "kappa",
                                 "algorithm"))
})

test_that("newdat and newsize must be specified correctly", {

        # newdat and m are not specified
        expect_error(neg_bin_pi(histdat=data.frame(qp_dat1)))

        # newdat and m are both specified
        expect_error(neg_bin_pi(histdat=data.frame(qp_dat1),
                                   newdat=data.frame(qp_dat2),
                                   newoffset=c(1,1,1,1,1)))

        # newdat is not a data frame
        expect_error(neg_bin_pi(histdat=data.frame(qp_dat1),
                                   newdat=c(1,2,3)))

        expect_error(neg_bin_pi(histdat=data.frame(qp_dat1),
                                newdat=data.frame(qp_dat2[,1])))

        newdat1 <- qp_dat2
        newdat1[1,2] <- "a"
        expect_error(neg_bin_pi(histdat=data.frame(qp_dat1),
                                newdat=newdat1))

        expect_warning(neg_bin_pi(histdat=data.frame(qp_dat1),
                                newdat=rbind(qp_dat2, qp_dat1)))

        newdat2 <- qp_dat2
        newdat2[1,1] <- 1.2
        expect_error(neg_bin_pi(histdat=data.frame(qp_dat1),
                                newdat=newdat2))

        expect_error(neg_bin_pi(histdat=data.frame(qp_dat1),
                                newoffset=c("a",1,1,1,1)))

        expect_warning(neg_bin_pi(histdat=data.frame(qp_dat1),
                                newoffset=1:12))


})


test_that("histdat must be specified correctly", {

        expect_error(neg_bin_pi(histdat=c(1,2,3),
                                   newoffset=c(1,1,1,1,1)))

        expect_error(neg_bin_pi(histdat=data.frame(qp_dat1[,1]),
                                newoffset=c(1,1,1,1,1)))

        hist_dat_1 <- qp_dat1
        hist_dat_1[1,1] <- 60.1
        expect_error(neg_bin_pi(histdat=hist_dat_1,
                                newoffset=c(1,1,1,1,1)))

        hist_dat_2 <- qp_dat2
        hist_dat_2[1,1] <- "a"
        expect_error(neg_bin_pi(histdat=hist_dat_2,
                                newoffset=c(1,1,1,1,1)))

        hist_dat_3 <- qp_dat2
        hist_dat_3[1,2] <- "a"
        expect_error(neg_bin_pi(histdat=hist_dat_3,
                                newoffset=c(1,1,1,1,1)))

        hist_dat_4 <- qp_dat2
        hist_dat_4[1:3,1] <- 0
        expect_error(neg_bin_pi(histdat=hist_dat_4,
                                newoffset=c(1,1,1,1,1)))

})


test_that("alternative", {

        # alternative is wrong
        expect_error(neg_bin_pi(histdat=data.frame(qp_dat1),
                                   newoffset=c(1,1,1,1,1),
                                   alternative="opper"))
})


test_that("algorithm", {

        # alternative is wrong
        expect_error(neg_bin_pi(histdat=data.frame(qp_dat1),
                                newoffset=c(1,1,1,1,1),
                                algorithm="MS23"))
})




