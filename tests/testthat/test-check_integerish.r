test_that("detects correct type", {
  expect_null(check_integerish(1L))
  expect_null(check_integerish(bare(1L)))
  expect_null(check_integerish(NA_integer_))
  expect_null(check_integerish(1.0))
  expect_error(check_integerish(1.1))
  expect_error(check_integerish(TRUE))
  expect_error(check_integerish(NULL))
  expect_error(check_integerish(NA))
  x <- structure(1L, class = "my_integerish")
  expect_error(check_integerish(bare(x)))

  expect_null(check_scalar_integerish(1L))
  expect_null(check_scalar_integerish(bare(1L)))
  expect_null(check_scalar_integerish(NA_integer_))
  expect_null(check_scalar_integerish(1.0))
  expect_error(check_scalar_integerish(1.1))
  expect_error(check_scalar_integerish(TRUE))
  expect_error(check_scalar_integerish(NULL))
  expect_error(check_scalar_integerish(NA))
  expect_error(check_scalar_integerish(bare(x)))
})

test_that("n arg checks length correctly", {
  expect_null(check_integerish(c(1L, 2L), n = 2))
  expect_error(check_integerish(c(1L, 2L), n = 1))
  expect_error(check_integerish(c(1L, 2L), n = 3))

  expect_null(check_integerish(c(1L, 2L), n = at_least(1)))
  expect_error(check_integerish(c(1L, 2L), n = at_least(3)))
  expect_null(check_integerish(c(1L, 2L), n = at_most(3)))
  expect_error(check_integerish(c(1L, 2L), n = at_most(1)))
  expect_null(check_integerish(c(1L, 2L), n = in_range(1, 3)))
  expect_error(check_integerish(c(1L, 2L), n = in_range(3, 5)))

  expect_null(check_scalar_integerish(1L))
  expect_error(check_scalar_integerish(c(1L, 2L)))
})

test_that("finite and allow_null work correctly", {
  expect_null(check_integerish(c(1L, 2L, NA_integer_), finite = FALSE))
  expect_error(check_integerish(c(1L, 2L, NA_integer_), finite = TRUE))
  expect_null(check_integerish(NULL, allow_null = TRUE))
  expect_error(check_integerish(NULL, allow_null = FALSE))

  expect_null(check_scalar_integerish(NA_integer_, finite = FALSE))
  expect_error(check_scalar_integerish(NA_integer_, finite = TRUE))
  expect_null(check_scalar_integerish(NULL, allow_null = TRUE))
  expect_error(check_scalar_integerish(NULL, allow_null = FALSE))

  expect_snapshot(error = TRUE, {
    check_integerish(c(1L, 2L, NA_integer_), finite = TRUE)
    check_integerish(NULL, allow_null = FALSE)
    check_scalar_integerish(NA_integer_, finite = TRUE)
    check_scalar_integerish(NULL, allow_null = FALSE)
  })
})

test_that("error shows type problem preferentially", {
  expect_snapshot(error = TRUE, {
    check_integerish(1.1, n = 2)
    check_integerish(bare(structure(1L, class = "my_integerish")), n = 2)
    check_integerish(c(1.1, 2.2), n = 1)
    check_scalar_integerish(c(1.1, 2.2))
    check_scalar_integerish(bare(structure(c(1L, 2L), class = "my_integerish")))
  })
})

test_that("error shows length problem when types match", {
  expect_snapshot(error = TRUE, {
    check_integerish(c(1L, 2L), n = 1)
    check_scalar_integerish(c(1L, 2L))
  })
})

test_that("arg is shown in error", {
  expect_snapshot(error = TRUE, {
    x <- 1.1
    check_integerish(x)
    check_scalar_integerish(x)
    check_integerish(x, n = 2, arg = "my_arg")
    check_scalar_integerish(x, arg = "my_arg")
  })
})

test_that("call is shown in error", {
  expect_snapshot(error = TRUE, {
    f <- function() {
      check_integerish(1.1)
    }
    fs <- function() {
      check_scalar_integerish(1.1)
    }
    f()
    fs()
  })
})

test_that("dots passed to cli_abort/abort", {
  expect_snapshot(error = TRUE, {
    check_integerish(1.1, footer = "Custom footer")
    check_scalar_integerish(1.1, footer = "Custom footer")
  })
})

test_that(".envir doesn't interfere", {
  expect_snapshot(error = TRUE, {
    e <- environment()
    e$arg <- "my_arg"
    check_integerish(1.1, .envir = e)
    check_scalar_integerish(1.1, .envir = e)
  })
})
