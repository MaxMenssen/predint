test_that("bisection", {

        mean_list  <- vector("list", length=10000)
        se_list <- vector("list", length=10000)
        fut_obs_list <- vector("list", length=10000)

        for(i in 1:length(fut_obs_list)){
                mean_list[[i]] <- rep(rnorm(1), 5)
                se_list[[i]] <- rep(rnorm(1, 1, 0.2), 5)
                fut_obs_list[[i]] <- rnorm(5)
        }

        # y_star_hat has to be a list
        expect_error(bisection(y_star_hat="mean_list",
                               pred_se=se_list,
                               y_star=fut_obs_list,
                               alternative="both",
                               quant_min=0.1,
                               quant_max=10,
                               n_bisec=10,
                               tol=0.03,
                               alpha=0.05))

        # pred_se has to be a list
        expect_error(bisection(y_star_hat=mean_list,
                               pred_se="se_list",
                               y_star=fut_obs_list,
                               alternative="both",
                               quant_min=0.1,
                               quant_max=10,
                               n_bisec=10,
                               tol=0.03,
                               alpha=0.05))

        # y_star has to be a list
        expect_error(bisection(y_star_hat=mean_list,
                               pred_se=se_list,
                               y_star="fut_obs_list",
                               alternative="both",
                               quant_min=0.1,
                               quant_max=10,
                               n_bisec=10,
                               tol=0.03,
                               alpha=0.05))

        # all three lists have to have the same length
        expect_error(bisection(y_star_hat=mean_list,
                               pred_se=se_list,
                               y_star=fut_obs_list[1:3],
                               alternative="both",
                               quant_min=0.1,
                               quant_max=10,
                               n_bisec=10,
                               tol=0.03,
                               alpha=0.05))

        # all elements of y_star_hat have to be of length M
        mean_list_1 <- mean_list
        mean_list_1[[1]] <- 1
        expect_error(bisection(y_star_hat=mean_list_1,
                               pred_se=se_list,
                               y_star=fut_obs_list,
                               alternative="both",
                               quant_min=0.1,
                               quant_max=10,
                               n_bisec=10,
                               tol=0.03,
                               alpha=0.05))

        # # all elements of pred_se have to be of length M
        se_list_1 <- se_list
        se_list_1[[1]] <- 1
        expect_error(bisection(y_star_hat=mean_list,
                               pred_se=se_list_1,
                               y_star=fut_obs_list,
                               alternative="both",
                               quant_min=0.1,
                               quant_max=10,
                               n_bisec=10,
                               tol=0.03,
                               alpha=0.05))

        # all elements of y_star have to be of length M
        fut_obs_list_1 <- fut_obs_list
        fut_obs_list_1[[1]] <- 1
        expect_error(bisection(y_star_hat=mean_list,
                               pred_se=se_list,
                               y_star=fut_obs_list_1,
                               alternative="both",
                               quant_min=0.1,
                               quant_max=10,
                               n_bisec=10,
                               tol=0.03,
                               alpha=0.05))

        # M has to be the same for all three lists
        # mean_list_2  <- vector("list", length=5)
        # se_list_2 <- vector("list", length=5)
        # fut_obs_list_2 <- vector("list", length=5)
        #
        # var(c(unique(sapply(mean_list_2, length)),
        #       unique(sapply(se_list_2, length)),
        #       unique(sapply(fut_obs_list_2, length)))) !=0
        #
        # for(i in 1:length(fut_obs_list_2)){
        #         mean_list_2[[i]] <- rep(rnorm(1), 2)
        #         se_list_2[[i]] <- rep(rnorm(1, 1, 0.2), 3)
        #         fut_obs_list_2[[i]] <- rnorm(5)
        # }
        #
        # expect_error(bisection(y_star_hat=mean_list_2,
        #                        pred_se=se_list_2,
        #                        y_star=fut_obs_list_2,
        #                        alternative="both",
        #                        quant_min=0.1,
        #                        quant_max=10,
        #                        n_bisec=10,
        #                        tol=0.03,
        #                        alpha=0.05))

        # alternative must be defined
        expect_error(bisection(y_star_hat=mean_list,
                               pred_se=se_list,
                               y_star=fut_obs_list,
                               alternative="bothe",
                               quant_min=0.1,
                               quant_max=10,
                               n_bisec=10,
                               tol=0.03,
                               alpha=0.05))

        # quant_min needs to be numeric
        expect_error(bisection(y_star_hat=mean_list,
                               pred_se=se_list,
                               y_star=fut_obs_list,
                               alternative="both",
                               quant_min="0.1",
                               quant_max=10,
                               n_bisec=10,
                               tol=0.03,
                               alpha=0.05))

        # quant_max needs to be numeric
        expect_error(bisection(y_star_hat=mean_list,
                               pred_se=se_list,
                               y_star=fut_obs_list,
                               alternative="both",
                               quant_min=0.1,
                               quant_max="10",
                               n_bisec=10,
                               tol=0.03,
                               alpha=0.05))

        # n_bisec needs ot be an integer number
        expect_error(bisection(y_star_hat=mean_list,
                               pred_se=se_list,
                               y_star=fut_obs_list,
                               alternative="both",
                               quant_min=0.1,
                               quant_max=10,
                               n_bisec="10",
                               tol=0.03,
                               alpha=0.05))

        expect_error(bisection(y_star_hat=mean_list,
                               pred_se=se_list,
                               y_star=fut_obs_list,
                               alternative="both",
                               quant_min=0.1,
                               quant_max=10,
                               n_bisec=10.1,
                               tol=0.03,
                               alpha=0.05))

        # tolerance needs to bve a number
        expect_error(bisection(y_star_hat=mean_list,
                               pred_se=se_list,
                               y_star=fut_obs_list,
                               alternative="both",
                               quant_min=0.1,
                               quant_max=10,
                               n_bisec=10,
                               tol="0.03",
                               alpha=0.05))

        # Tolerance needs to be bigger than 0
        expect_error(bisection(y_star_hat=mean_list,
                               pred_se=se_list,
                               y_star=fut_obs_list,
                               alternative="both",
                               quant_min=0.1,
                               quant_max=10,
                               n_bisec=10,
                               tol=0,
                               alpha=0.05))

        # Tolerance needs to be small to yield accurate results
        expect_warning(bisection(y_star_hat=mean_list,
                               pred_se=se_list,
                               y_star=fut_obs_list,
                               alternative="both",
                               quant_min=0.1,
                               quant_max=10,
                               n_bisec=10,
                               tol=0.011,
                               alpha=0.05,
                               traceplot=FALSE))

        # traceplote needs to be TRUE or FALSE
        expect_error(bisection(y_star_hat=mean_list,
                                 pred_se=se_list,
                                 y_star=fut_obs_list,
                                 alternative="both",
                                 quant_min=0.1,
                                 quant_max=10,
                                 n_bisec=10,
                                 tol=0.001,
                                 alpha=0.05,
                                 traceplot=1))

        # Output must be numeric
        expect_true(is.numeric(bisection(y_star_hat=mean_list,
                               pred_se=se_list,
                               y_star=fut_obs_list,
                               alternative="both",
                               quant_min=0.1,
                               quant_max=10,
                               n_bisec=10,
                               tol=0.001,
                               alpha=0.05,
                               traceplot=FALSE)))

        expect_true(is.numeric(bisection(y_star_hat=mean_list,
                                         pred_se=se_list,
                                         y_star=fut_obs_list,
                                         alternative="upper",
                                         quant_min=0.1,
                                         quant_max=10,
                                         n_bisec=10,
                                         tol=0.001,
                                         alpha=0.05,
                                         traceplot=FALSE)))

        expect_true(is.numeric(bisection(y_star_hat=mean_list,
                                         pred_se=se_list,
                                         y_star=fut_obs_list,
                                         alternative="lower",
                                         quant_min=0.1,
                                         quant_max=10,
                                         n_bisec=10,
                                         tol=0.001,
                                         alpha=0.05,
                                         traceplot=FALSE)))

        # Output must be one number
        expect_true(1==length(bisection(y_star_hat=mean_list,
                                         pred_se=se_list,
                                         y_star=fut_obs_list,
                                         alternative="both",
                                         quant_min=0.1,
                                         quant_max=10,
                                         n_bisec=10,
                                         tol=0.001,
                                         alpha=0.05,
                                         traceplot=FALSE)))

        expect_true(1==length(bisection(y_star_hat=mean_list,
                                         pred_se=se_list,
                                         y_star=fut_obs_list,
                                         alternative="upper",
                                         quant_min=0.1,
                                         quant_max=10,
                                         n_bisec=10,
                                         tol=0.001,
                                         alpha=0.05,
                                         traceplot=FALSE)))

        expect_true(1==length(bisection(y_star_hat=mean_list,
                                         pred_se=se_list,
                                         y_star=fut_obs_list,
                                         alternative="lower",
                                         quant_min=0.1,
                                         quant_max=10,
                                         n_bisec=10,
                                         tol=0.001,
                                         alpha=0.05,
                                         traceplot=FALSE)))
})

