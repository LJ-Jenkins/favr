test_that("detects correct type", {
  expect_null(check_raw(raw(1)))
  expect_null(check_raw(bare(raw(1))))
  expect_error(check_raw(1.1))
  expect_error(check_raw(TRUE))
  expect_error(check_raw(NULL))
  expect_error(check_raw(NA))
  x <- structure(raw(1), class = "my_class")
  expect_error(check_raw(bare(x)))

  expect_null(check_scalar_raw(raw(1)))
  expect_null(check_scalar_raw(bare(raw(1))))
  expect_error(check_scalar_raw(1.1))
  expect_error(check_scalar_raw(TRUE))
  expect_error(check_scalar_raw(NULL))
  expect_error(check_scalar_raw(NA))
  expect_error(check_scalar_raw(bare(x)))
})

test_that("n arg checks length correctly", {
  expect_null(check_raw(raw(2), n = 2))
  expect_error(check_raw(raw(2), n = 1))
  expect_error(check_raw(raw(2), n = 3))

  expect_null(check_raw(raw(2), n = at_least(1)))
  expect_error(check_raw(raw(2), n = at_least(3)))
  expect_null(check_raw(raw(2), n = at_most(3)))
  expect_error(check_raw(raw(2), n = at_most(1)))
  expect_null(check_raw(raw(2), n = in_range(1, 3)))
  expect_error(check_raw(raw(2), n = in_range(3, 5)))

  expect_null(check_scalar_raw(raw(1)))
  expect_error(check_scalar_raw(raw(2)))
})

test_that("allow_null works correctly", {
  expect_null(check_raw(NULL, allow_null = TRUE))
  expect_error(check_raw(NULL, allow_null = FALSE))

  expect_null(check_scalar_raw(NULL, allow_null = TRUE))
  expect_error(check_scalar_raw(NULL, allow_null = FALSE))

  expect_snapshot(error = TRUE, {
    check_raw(NULL, allow_null = FALSE)
    check_scalar_raw(NULL, allow_null = FALSE)
  })
})

test_that("error shows type problem preferentially", {
  expect_snapshot(error = TRUE, {
    check_raw(1.1, n = 2)
    check_raw(c(1.1, 2L), n = 1)
    check_raw(bare(structure(raw(1), class = "my_class")), n = 2)
    check_scalar_raw(c(1.1, 2L))
    check_scalar_raw(bare(structure(raw(2), class = "my_class")))
  })
})

test_that("error shows length problem when types match", {
  expect_snapshot(error = TRUE, {
    check_raw(raw(2), n = 1)
    check_scalar_raw(raw(2))
  })
})

test_that("arg is shown in error", {
  expect_snapshot(error = TRUE, {
    x <- 1.1
    check_raw(x)
    check_scalar_raw(x)
    check_raw(x, n = 2, arg = "my_arg")
    check_scalar_raw(x, arg = "my_arg")
  })
})

test_that("call is shown in error", {
  expect_snapshot(error = TRUE, {
    f <- function() {
      check_raw(1.1)
    }
    fs <- function() {
      check_scalar_raw(1.1)
    }
    f()
    fs()
  })
})

test_that("dots passed to cli_abort/abort", {
  expect_snapshot(error = TRUE, {
    check_raw(1.1, footer = "Custom footer")
    check_scalar_raw(1.1, footer = "Custom footer")
  })
})

test_that(".envir doesn't interfere", {
  expect_snapshot(error = TRUE, {
    e <- environment()
    e$arg <- "my_arg"
    check_raw(1.1, .envir = e)
    check_scalar_raw(1.1, .envir = e)
  })
})
