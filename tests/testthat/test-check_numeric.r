test_that("detects correct type", {
  expect_null(check_numeric(1.1))
  expect_null(check_numeric(bare(1.1)))
  expect_null(check_numeric(NA_real_))
  expect_null(check_numeric(1L))
  expect_null(check_numeric(NA_integer_))
  expect_error(check_numeric(TRUE))
  expect_error(check_numeric(NULL))
  expect_error(check_numeric(NA))
  x <- structure(1.1, class = "my_numeric")
  expect_error(check_numeric(bare(x)))

  expect_null(check_scalar_numeric(1.1))
  expect_null(check_scalar_numeric(bare(1.1)))
  expect_null(check_scalar_numeric(NA_real_))
  expect_null(check_scalar_numeric(1L))
  expect_null(check_scalar_numeric(NA_integer_))
  expect_error(check_scalar_numeric(TRUE))
  expect_error(check_scalar_numeric(NULL))
  expect_error(check_scalar_numeric(NA))
  expect_error(check_scalar_numeric(bare(x)))
})

test_that("n arg checks length correctly", {
  expect_null(check_numeric(c(1.1, 2.2), n = 2))
  expect_error(check_numeric(c(1.1, 2.2), n = 1))
  expect_error(check_numeric(c(1.1, 2.2), n = 3))

  expect_null(check_numeric(c(1.1, 2.2), n = at_least(1)))
  expect_error(check_numeric(c(1.1, 2.2), n = at_least(3)))
  expect_null(check_numeric(c(1.1, 2.2), n = at_most(3)))
  expect_error(check_numeric(c(1.1, 2.2), n = at_most(1)))
  expect_null(check_numeric(c(1.1, 2.2), n = in_range(1, 3)))
  expect_error(check_numeric(c(1.1, 2.2), n = in_range(3, 5)))

  expect_null(check_scalar_numeric(1.1))
  expect_error(check_scalar_numeric(c(1.1, 2.2)))
})

test_that("allow_null works correctly", {
  expect_null(check_numeric(NULL, allow_null = TRUE))
  expect_error(check_numeric(NULL, allow_null = FALSE))

  expect_null(check_scalar_numeric(NULL, allow_null = TRUE))
  expect_error(check_scalar_numeric(NULL, allow_null = FALSE))

  expect_snapshot(error = TRUE, {
    check_numeric(NULL, allow_null = FALSE)
    check_scalar_numeric(NULL, allow_null = FALSE)
  })
})

test_that("finite works correctly", {
  expect_null(check_numeric(c(1.1, 2.2, Inf), finite = FALSE))
  expect_error(check_numeric(c(1.1, 2.2, Inf), finite = TRUE))
  expect_null(check_numeric(c(1.1, 2.2, NaN), finite = FALSE))
  expect_error(check_numeric(c(1.1, 2.2, NaN), finite = TRUE))

  expect_null(check_scalar_numeric(Inf, finite = FALSE))
  expect_error(check_scalar_numeric(Inf, finite = TRUE))
  expect_null(check_scalar_numeric(NaN, finite = FALSE))
  expect_error(check_scalar_numeric(NaN, finite = TRUE))

  expect_snapshot(error = TRUE, {
    check_numeric(c(1.1, 2.2, Inf), finite = TRUE)
    check_numeric(c(1.1, 2.2, NaN), finite = TRUE)
    check_scalar_numeric(Inf, finite = TRUE)
    check_scalar_numeric(NaN, finite = TRUE)
  })
})

test_that("error shows type problem preferentially", {
  expect_snapshot(error = TRUE, {
    check_numeric(list(1), n = 2)
    check_numeric(bare(structure(1.1, class = "my_numeric")), n = 2)
    check_numeric(c("a", "b"), n = 1)
    check_scalar_numeric(c("a", "b"))
    check_scalar_numeric(bare(structure(c(1.1, 2.2), class = "my_numeric")))
  })
})

test_that("error shows length problem when types match", {
  expect_snapshot(error = TRUE, {
    check_numeric(c(1.1, 2.2), n = 1)
    check_scalar_numeric(c(1.1, 2.2))
  })
})

test_that("arg is shown in error", {
  expect_snapshot(error = TRUE, {
    x <- 1L
    check_numeric(x)
    check_scalar_numeric(x)
    check_numeric(x, n = 2, arg = "my_arg")
    check_scalar_numeric(x, arg = "my_arg")
  })
})

test_that("call is shown in error", {
  expect_snapshot(error = TRUE, {
    f <- function() {
      check_numeric("a")
    }
    fs <- function() {
      check_scalar_numeric("a")
    }
    f()
    fs()
  })
})

test_that("dots passed to cli_abort/abort", {
  expect_snapshot(error = TRUE, {
    check_numeric("a", footer = "Custom footer")
    check_scalar_numeric("a", footer = "Custom footer")
  })
})

test_that(".envir doesn't interfere", {
  expect_snapshot(error = TRUE, {
    e <- environment()
    e$arg <- "my_arg"
    check_numeric("a", .envir = e)
    check_scalar_numeric("a", .envir = e)
  })
})
