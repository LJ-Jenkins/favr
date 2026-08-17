test_that("detects correct type", {
  expect_null(check_null(NULL))
  expect_error(check_null(1L))
  expect_error(check_null(TRUE))
  expect_error(check_null(NA))
  expect_error(check_null(bare(NULL)))
})

test_that("error shows type problem", {
  expect_snapshot(error = TRUE, {
    check_null(1L)
    check_null(c(1L, 2L))
  })
})

test_that("arg is shown in error", {
  expect_snapshot(error = TRUE, {
    x <- 1L
    check_null(x)
    check_null(x, arg = "my_arg")
  })
})

test_that("call is shown in error", {
  expect_snapshot(error = TRUE, {
    f <- function() {
      check_null(1L)
    }
    f()
  })
})

test_that("dots passed to cli_abort/abort", {
  expect_snapshot(error = TRUE, {
    check_null(1L, footer = "Custom footer")
  })
})

test_that(".envir doesn't interfere", {
  expect_snapshot(error = TRUE, {
    e <- environment()
    e$arg <- "my_arg"
    check_null(1L, .envir = e)
  })
})
