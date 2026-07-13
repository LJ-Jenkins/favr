test_that("detects correct type", {
  expect_null(check_integer(1L))
  expect_null(check_integer(bare(1L)))
  expect_null(check_integer(NA_integer_))
  expect_error(check_integer(1.1))
  expect_error(check_integer(TRUE))
  expect_error(check_integer(NULL))
  expect_error(check_integer(NA))
  x <- structure(1L, class = "my_integer")
  expect_error(check_integer(bare(x)))

  expect_null(check_scalar_integer(1L))
  expect_null(check_scalar_integer(bare(1L)))
  expect_null(check_scalar_integer(NA_integer_))
  expect_error(check_scalar_integer(1.1))
  expect_error(check_scalar_integer(TRUE))
  expect_error(check_scalar_integer(NULL))
  expect_error(check_scalar_integer(NA))
  expect_error(check_scalar_integer(bare(x)))
})

test_that("n arg checks length correctly", {
  expect_null(check_integer(c(1L, 2L), n = 2))
  expect_error(check_integer(c(1L, 2L), n = 1))
  expect_error(check_integer(c(1L, 2L), n = 3))

  expect_null(check_integer(c(1L, 2L), n = at_least(1)))
  expect_error(check_integer(c(1L, 2L), n = at_least(3)))
  expect_null(check_integer(c(1L, 2L), n = at_most(3)))
  expect_error(check_integer(c(1L, 2L), n = at_most(1)))
  expect_null(check_integer(c(1L, 2L), n = in_range(1, 3)))
  expect_error(check_integer(c(1L, 2L), n = in_range(3, 5)))

  expect_null(check_scalar_integer(1L))
  expect_error(check_scalar_integer(c(1L, 2L)))
})

test_that("allow_na and allow_null work correctly", {
  expect_null(check_integer(c(1L, 2L, NA_integer_), allow_na = TRUE))
  expect_error(check_integer(c(1L, 2L, NA_integer_), allow_na = FALSE))
  expect_null(check_integer(NULL, allow_null = TRUE))
  expect_error(check_integer(NULL, allow_null = FALSE))

  expect_null(check_scalar_integer(NA_integer_, allow_na = TRUE))
  expect_error(check_scalar_integer(NA_integer_, allow_na = FALSE))
  expect_null(check_scalar_integer(NULL, allow_null = TRUE))
  expect_error(check_scalar_integer(NULL, allow_null = FALSE))

  expect_snapshot(error = TRUE, {
    check_integer(c(1L, 2L, NA_integer_), allow_na = FALSE)
    check_integer(NULL, allow_null = FALSE)
    check_scalar_integer(NA_integer_, allow_na = FALSE)
    check_scalar_integer(NULL, allow_null = FALSE)
  })
})

test_that("error shows type problem preferentially", {
  expect_snapshot(error = TRUE, {
    check_integer(1.1, n = 2)
    check_integer(bare(structure(1L, class = "my_integer")), n = 2)
    check_integer(c(1.1, 2.2), n = 1)
    check_scalar_integer(c(1.1, 2.2))
    check_scalar_integer(bare(structure(c(1L, 2L), class = "my_integer")))
  })
})

test_that("error shows length problem when types match", {
  expect_snapshot(error = TRUE, {
    check_integer(c(1L, 2L), n = 1)
    check_scalar_integer(c(1L, 2L))
  })
})

test_that("arg is shown in error", {
  expect_snapshot(error = TRUE, {
    x <- 1.1
    check_integer(x)
    check_scalar_integer(x)
    check_integer(x, n = 2, arg = "my_arg")
    check_scalar_integer(x, arg = "my_arg")
  })
})

test_that("call is shown in error", {
  expect_snapshot(error = TRUE, {
    f <- function() {
      check_integer(1.1)
    }
    fs <- function() {
      check_scalar_integer(1.1)
    }
    f()
    fs()
  })
})

test_that("dots evaluated and passed to cli_abort/abort", {
  expect_snapshot(error = TRUE, {
    check_integer(1.1, footer = "Custom footer")
    check_scalar_integer(1.1, footer = "Custom footer")
  })
})

test_that(".envir is silently overriden", {
  expect_snapshot(error = TRUE, {
    e <- environment()
    e$arg <- "my_arg"
    check_integer(1.1, .envir = e)
    check_scalar_integer(1.1, .envir = e)
  })
})
