test_that("stop if ncol dat !2", {

        # Data frame
        dat1 <- data.frame(x=c(0,0,0))
        dat2 <- data.frame(x=c(0,0,0), y=c(10, 10, 10), z=c(10, 10, 10))

        # Tests
        expect_error(pi_rho_est(dat=dat1))
        expect_error(pi_rho_est(dat=dat2))
})


test_that("adjustment works if all x are 0", {

        # Data frame
        dat <- data.frame(x=c(0,0,0), y=c(10, 10, 10))

        # pi and rho
        pi_rho <- suppressWarnings(pi_rho_est(dat=dat))

  # Tests
  expect_equal(round(unname(pi_rho[1]), 4), 0.0167)
  expect_equal(round(unname(pi_rho[2]), 4), -0.0556)
})


test_that("adjustment works if all y are 0", {

        # Data frame
        dat <- data.frame(x=c(10, 10, 10), y=c(0,0,0))

        # pi and rho
        pi_rho <- suppressWarnings(pi_rho_est(dat=dat))

        # Tests
        expect_equal(round(unname(pi_rho[1]), 4), 0.9833)
        expect_equal(round(unname(pi_rho[2]), 4), -0.0556 )
})

test_that("warning if all x are 0", {

        # Data frame
        dat <- data.frame(x=c(0,0,0), y=c(10, 10, 10))

        expect_warning(pi_rho_est(dat=dat))
})


test_that("warning if all x are 0", {

        # Data frame
        dat <- data.frame(x=c(10, 10, 10), y=c(0,0,0))

        expect_warning(pi_rho_est(dat=dat))
})

test_that("output must be a vector of length 2", {

        # Data frame
        dat <- data.frame(x=c(10, 10, 10), y=c(0,0,0))

        # pi and rho
        pi_rho <- suppressWarnings(pi_rho_est(dat=dat))

        # Tests
        expect_equal(length(pi_rho), 2)
})
