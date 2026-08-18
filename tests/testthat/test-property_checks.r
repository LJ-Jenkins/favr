test_that("detects forbidden properties", {
  x <- c(1, 2, 3, NA)
  expect_null(check_length(x, 4))
  expect_error(check_length(x, 3))
  expect_null(check_size(x, 4))
  expect_error(check_size(x, 3))
  expect_null(check_non_empty(x))
  expect_error(check_non_empty(NULL))
  expect_error(check_non_empty(numeric(0)))

  x <- data.frame(a = 1:3, b = 4:6)
  expect_null(check_nrow(x, 3))
  expect_error(check_nrow(x, 2))
  expect_null(check_ncol(x, 2))
  expect_error(check_ncol(x, 3))
  expect_null(check_size(x, 3))
  expect_error(check_size(x, 2))

  x <- c(1, 2, 3)
  expect_error(check_named(x))
  names(x) <- c("a", "b", "a")
  expect_null(check_named(x))
  expect_error(check_named(x, unique = TRUE))
  names(x) <- c("a", "b", "")
  expect_null(check_named(x))
  expect_error(check_named(x, allow_empty = FALSE))
  names(x) <- c("a", "", "")
  expect_error(check_named(x, unique = TRUE))
})

test_that("allow_null arg works correctly", {
  expect_null(check_length(NULL, 1, allow_null = TRUE))
  expect_error(check_length(NULL, 1, allow_null = FALSE))

  expect_null(check_size(NULL, 1, allow_null = TRUE))
  expect_error(check_size(NULL, 1, allow_null = FALSE))

  expect_null(check_nrow(NULL, 1, allow_null = TRUE))
  expect_error(check_nrow(NULL, 1, allow_null = FALSE))

  expect_null(check_ncol(NULL, 1, allow_null = TRUE))
  expect_error(check_ncol(NULL, 1, allow_null = FALSE))

  expect_error(check_non_empty(NULL))
  expect_error(check_named(NULL))
})

test_that("type not checked - errors from incorrect types", {
  expect_snapshot(error = TRUE, {
    check_size(mean)
    check_nrow(mean)
    check_ncol(mean)
  })
})

test_that("arg is shown in error", {
  expect_snapshot(error = TRUE, {
    x <- 1:2
    check_length(x, 1)
    check_length(x, 1, arg = "my_arg")
    check_size(x, 1)
    check_size(x, 1, arg = "my_arg")
    x <- data.frame(x = 1:2)
    check_nrow(x, 1)
    check_nrow(x, 1, arg = "my_arg")
    check_ncol(x, 1)
    check_ncol(x, 1, arg = "my_arg")
    x <- NULL
    check_non_empty(x)
    check_non_empty(x, arg = "my_arg")
    check_named(x)
    check_named(x, arg = "my_arg")
  })
})

test_that("call is shown in error", {
  expect_snapshot(error = TRUE, {
    f <- function() {
      check_length(1, 2)
    }
    f()

    f <- function() {
      check_size(1, 2)
    }
    f()

    f <- function() {
      check_nrow(data.frame(x = 1), 2)
    }
    f()

    f <- function() {
      check_ncol(data.frame(x = 1), 2)
    }
    f()

    f <- function() {
      check_non_empty(NULL)
    }
    f()

    f <- function() {
      check_named(1:2)
    }
    f()
  })
})

test_that("dots passed to cli_abort/abort", {
  expect_snapshot(error = TRUE, {
    check_length(1, 2, footer = "Custom footer")
    check_size(1, 2, footer = "Custom footer")
    check_nrow(data.frame(x = 1), 2, footer = "Custom footer")
    check_ncol(data.frame(x = 1), 2, footer = "Custom footer")
    check_non_empty(numeric(0), footer = "Custom footer")
    check_named(1:2, footer = "Custom footer")
  })
})

test_that(".envir doesn't interfere", {
  expect_snapshot(error = TRUE, {
    e <- environment()
    e$arg <- "my_arg"
    check_length(1, 2, .envir = e)
    check_size(1, 2, .envir = e)
    check_nrow(data.frame(x = 1), 2, .envir = e)
    check_ncol(data.frame(x = 1), 2, .envir = e)
    check_non_empty(numeric(0), .envir = e)
    check_named(1:2, .envir = e)
  })
})
