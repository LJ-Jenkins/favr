test_that("detects forbidden values", {
  x <- c(1, 2, NA)
  y <- c(1, 2, 3)
  expect_error(check_no_na(x))
  expect_null(check_no_na(y))
  expect_error(check_finite(x))
  expect_null(check_finite(y))
  x <- c(1, 2, Inf)
  expect_error(check_finite(x))
  expect_null(check_no_na(x))

  x <- c("a", "b", NA, "")
  y <- c("a", "b", NA)
  expect_error(check_nzchar(x))
  expect_null(check_nzchar(y))
  x <- c("a", "b", NA, " ")
  expect_null(check_nzchar(x))
  expect_error(check_nzchar(x, allow_all_ws = FALSE))
})

test_that("allow_null arg works correctly", {
  expect_null(check_no_na(NULL, allow_null = TRUE))
  expect_error(check_no_na(NULL, allow_null = FALSE))

  expect_null(check_finite(NULL, allow_null = TRUE))
  expect_error(check_finite(NULL, allow_null = FALSE))

  expect_null(check_nzchar(NULL, allow_null = TRUE))
  expect_error(check_nzchar(NULL, allow_null = FALSE))
})

test_that("type not checked - base function show type errors", {
  expect_snapshot(error = TRUE, {
    check_no_na(mean)
    check_finite(mean)
    check_nzchar(mean)
  })
})

test_that("arg is shown in error", {
  expect_snapshot(error = TRUE, {
    x <- NA
    check_no_na(x)
    check_no_na(x, arg = "my_arg")
    check_finite(x)
    check_finite(x, arg = "my_arg")
    x <- ""
    check_nzchar(x)
    check_nzchar(x, arg = "my_arg")
    x <- "  "
    check_nzchar(x, allow_all_ws = FALSE)
    check_nzchar(x, allow_all_ws = FALSE, arg = "my_arg")
  })
})

test_that("call is shown in error", {
  expect_snapshot(error = TRUE, {
    f <- function() {
      check_no_na(NA)
    }
    f()

    f <- function() {
      check_finite(NA)
    }
    f()

    f <- function() {
      check_nzchar("")
    }
    f()

    f <- function() {
      check_nzchar(" ", allow_all_ws = FALSE)
    }
    f()
  })
})

test_that("dots passed to cli_abort/abort", {
  expect_snapshot(error = TRUE, {
    check_no_na(NA, footer = "Custom footer")
    check_finite(NA, footer = "Custom footer")
    check_nzchar("", footer = "Custom footer")
    check_nzchar(" ", allow_all_ws = FALSE, footer = "Custom footer")
  })
})

test_that(".envir doesn't interfere", {
  expect_snapshot(error = TRUE, {
    e <- environment()
    e$arg <- "my_arg"
    check_no_na(NA, .envir = e)
    check_finite(NA, .envir = e)
    check_nzchar("", .envir = e)
    check_nzchar(" ", allow_all_ws = FALSE, .envir = e)
  })
})
