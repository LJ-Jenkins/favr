test_that("detects correct type", {
  expect_null(check_double(1.1))
  expect_null(check_double(bare(1.1)))
  expect_null(check_double(NA_real_))
  expect_error(check_double(1L))
  expect_error(check_double(TRUE))
  expect_error(check_double(NULL))
  expect_error(check_double(NA))
  x <- structure(1.1, class = "my_double")
  expect_error(check_double(bare(x)))

  expect_null(check_scalar_double(1.1))
  expect_null(check_scalar_double(bare(1.1)))
  expect_null(check_scalar_double(NA_real_))
  expect_error(check_scalar_double(1L))
  expect_error(check_scalar_double(TRUE))
  expect_error(check_scalar_double(NULL))
  expect_error(check_scalar_double(NA))
  expect_error(check_scalar_double(bare(x)))
})

test_that("n arg checks length correctly", {
  expect_null(check_double(c(1.1, 2.2), n = 2))
  expect_error(check_double(c(1.1, 2.2), n = 1))
  expect_error(check_double(c(1.1, 2.2), n = 3))

  expect_null(check_double(c(1.1, 2.2), n = at_least(1)))
  expect_error(check_double(c(1.1, 2.2), n = at_least(3)))
  expect_null(check_double(c(1.1, 2.2), n = at_most(3)))
  expect_error(check_double(c(1.1, 2.2), n = at_most(1)))
  expect_null(check_double(c(1.1, 2.2), n = in_range(1, 3)))
  expect_error(check_double(c(1.1, 2.2), n = in_range(3, 5)))

  expect_null(check_scalar_double(1.1))
  expect_error(check_scalar_double(c(1.1, 2.2)))
})

test_that("allow_null works correctly", {
  expect_null(check_double(NULL, allow_null = TRUE))
  expect_error(check_double(NULL, allow_null = FALSE))

  expect_null(check_scalar_double(NULL, allow_null = TRUE))
  expect_error(check_scalar_double(NULL, allow_null = FALSE))

  expect_snapshot(error = TRUE, {
    check_double(NULL, allow_null = FALSE)
    check_scalar_double(NULL, allow_null = FALSE)
  })
})

test_that("finite works correctly", {
  expect_null(check_double(c(1.1, 2.2, Inf), finite = FALSE))
  expect_error(check_double(c(1.1, 2.2, Inf), finite = TRUE))
  expect_null(check_double(c(1.1, 2.2, NaN), finite = FALSE))
  expect_error(check_double(c(1.1, 2.2, NaN), finite = TRUE))

  expect_null(check_scalar_double(Inf, finite = FALSE))
  expect_error(check_scalar_double(Inf, finite = TRUE))
  expect_null(check_scalar_double(NaN, finite = FALSE))
  expect_error(check_scalar_double(NaN, finite = TRUE))

  expect_snapshot(error = TRUE, {
    check_double(c(1.1, 2.2, Inf), finite = TRUE)
    check_double(c(1.1, 2.2, NaN), finite = TRUE)
    check_scalar_double(Inf, finite = TRUE)
    check_scalar_double(NaN, finite = TRUE)
  })
})

test_that("error shows type problem preferentially", {
  expect_snapshot(error = TRUE, {
    check_double(1L, n = 2)
    check_double(bare(structure(1.1, class = "my_double")), n = 2)
    check_double(c(1L, 2L), n = 1)
    check_scalar_double(c(1L, 2L))
    check_scalar_double(bare(structure(c(1.1, 2.2), class = "my_double")))
  })
})

test_that("error shows length problem when types match", {
  expect_snapshot(error = TRUE, {
    check_double(c(1.1, 2.2), n = 1)
    check_scalar_double(c(1.1, 2.2))
  })
})

test_that("arg is shown in error", {
  expect_snapshot(error = TRUE, {
    x <- 1L
    check_double(x)
    check_scalar_double(x)
    check_double(x, n = 2, arg = "my_arg")
    check_scalar_double(x, arg = "my_arg")
  })
})

test_that("call is shown in error", {
  expect_snapshot(error = TRUE, {
    f <- function() {
      check_double(1L)
    }
    fs <- function() {
      check_scalar_double(1L)
    }
    f()
    fs()
  })
})

test_that("dots evaluated and passed to cli_abort/abort", {
  expect_snapshot(error = TRUE, {
    check_double(1L, footer = "Custom footer")
    check_scalar_double(1L, footer = "Custom footer")
  })
})

test_that(".envir is silently overriden", {
  expect_snapshot(error = TRUE, {
    e <- environment()
    e$arg <- "my_arg"
    check_double(1L, .envir = e)
    check_scalar_double(1L, .envir = e)
  })
})
