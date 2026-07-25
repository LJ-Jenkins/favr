test_that("detects correct type", {
  expect_null(check_bytes(bytes(1)))
  expect_null(check_bytes(bare(bytes(1))))
  expect_error(check_bytes(1.1))
  expect_error(check_bytes(TRUE))
  expect_error(check_bytes(NULL))
  expect_error(check_bytes(NA))
  x <- structure(bytes(1), class = "my_bytes")
  expect_error(check_bytes(bare(x)))

  expect_null(check_scalar_bytes(bytes(1)))
  expect_null(check_scalar_bytes(bare(bytes(1))))
  expect_error(check_scalar_bytes(1.1))
  expect_error(check_scalar_bytes(TRUE))
  expect_error(check_scalar_bytes(NULL))
  expect_error(check_scalar_bytes(NA))
  expect_error(check_scalar_bytes(bare(x)))
})

test_that("n arg checks length correctly", {
  expect_null(check_bytes(bytes(1, 2), n = 2))
  expect_error(check_bytes(bytes(1, 2), n = 1))
  expect_error(check_bytes(bytes(1, 2), n = 3))

  expect_null(check_bytes(bytes(1, 2), n = at_least(1)))
  expect_error(check_bytes(bytes(1, 2), n = at_least(3)))
  expect_null(check_bytes(bytes(1, 2), n = at_most(3)))
  expect_error(check_bytes(bytes(1, 2), n = at_most(1)))
  expect_null(check_bytes(bytes(1, 2), n = in_range(1, 3)))
  expect_error(check_bytes(bytes(1, 2), n = in_range(3, 5)))

  expect_null(check_scalar_bytes(bytes(1)))
  expect_error(check_scalar_bytes(bytes(1, 2)))
})

test_that("allow_null workss correctly", {
  expect_null(check_bytes(NULL, allow_null = TRUE))
  expect_error(check_bytes(NULL, allow_null = FALSE))

  expect_null(check_scalar_bytes(NULL, allow_null = TRUE))
  expect_error(check_scalar_bytes(NULL, allow_null = FALSE))

  expect_snapshot(error = TRUE, {
    check_bytes(NULL, allow_null = FALSE)
    check_scalar_bytes(NULL, allow_null = FALSE)
  })
})

test_that("error shows type problem preferentially", {
  expect_snapshot(error = TRUE, {
    check_bytes(1.1, n = 2)
    check_bytes(bare(structure(bytes(1), class = "my_bytes")), n = 2)
    check_bytes(c(1.1, 2L), n = 1)
    check_scalar_bytes(c(1.1, 2L))
    check_scalar_bytes(bare(structure(bytes(1, 2), class = "my_bytes")))
  })
})

test_that("error shows length problem when types match", {
  expect_snapshot(error = TRUE, {
    check_bytes(bytes(1, 2), n = 1)
    check_scalar_bytes(bytes(1, 2))
  })
})

test_that("arg is shown in error", {
  expect_snapshot(error = TRUE, {
    x <- 1.1
    check_bytes(x)
    check_scalar_bytes(x)
    check_bytes(x, n = 2, arg = "my_arg")
    check_scalar_bytes(x, arg = "my_arg")
  })
})

test_that("call is shown in error", {
  expect_snapshot(error = TRUE, {
    f <- function() {
      check_bytes(1.1)
    }
    fs <- function() {
      check_scalar_bytes(1.1)
    }
    f()
    fs()
  })
})

test_that("dots passed to cli_abort/abort", {
  expect_snapshot(error = TRUE, {
    check_bytes(1.1, footer = "Custom footer")
    check_scalar_bytes(1.1, footer = "Custom footer")
  })
})

test_that(".envir doesn't interfere", {
  expect_snapshot(error = TRUE, {
    e <- environment()
    e$arg <- "my_arg"
    check_bytes(1.1, .envir = e)
    check_scalar_bytes(1.1, .envir = e)
  })
})
