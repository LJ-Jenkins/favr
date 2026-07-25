test_that("detects correct type", {
  x <- array()
  expect_error(check_array(1:12))
  expect_null(check_array(x))
  expect_null(check_array(bare(x)))
  class(x) <- c("my_array", class(x))
  expect_null(check_array(x))
  expect_error(check_array(bare(x)))

  x <- matrix()
  expect_error(check_matrix(1:12))
  expect_null(check_matrix(x))
  expect_null(check_matrix(bare(x)))
  class(x) <- c("my_array", class(x))
  expect_null(check_matrix(x))
  expect_error(check_matrix(bare(x)))
})

test_that("n arg checks length correctly", {
  x <- array(1:12, dim = c(3, 4))
  expect_null(check_array(x, n = 12))
  expect_null(check_array(x, n = at_least(10)))
  expect_error(check_array(x, n = 10))
  expect_error(check_array(x, n = at_least(13)))
  expect_error(check_array(x, n = at_most(11)))
  expect_error(check_array(x, n = in_range(13, 15)))

  x <- matrix(1:12, nrow = 3)
  expect_null(check_matrix(x, n = 12))
  expect_null(check_matrix(x, n = at_least(10)))
  expect_error(check_matrix(x, n = 10))
  expect_error(check_matrix(x, n = at_least(13)))
  expect_error(check_matrix(x, n = at_most(11)))
  expect_error(check_matrix(x, n = in_range(13, 15)))
})

test_that("nrow ncol args check size correctly", {
  x <- array(1:12, dim = c(3, 4))
  expect_null(check_array(x, nrow = 3))
  expect_null(check_array(x, ncol = 4))
  expect_null(check_array(x, nrow = at_least(2)))
  expect_null(check_array(x, ncol = at_most(5)))
  expect_null(check_array(x, nrow = in_range(2, 4)))
  expect_error(check_array(x, nrow = 2))
  expect_error(check_array(x, ncol = 5))
  expect_error(check_array(x, nrow = at_least(4)))
  expect_error(check_array(x, ncol = at_most(3)))
  expect_error(check_array(x, nrow = in_range(4, 5)))

  x <- matrix(1:12, nrow = 3)
  expect_null(check_matrix(x, nrow = 3))
  expect_null(check_matrix(x, ncol = 4))
  expect_null(check_matrix(x, nrow = at_least(2)))
  expect_null(check_matrix(x, ncol = at_most(5)))
  expect_null(check_matrix(x, nrow = in_range(2, 4)))
  expect_error(check_matrix(x, nrow = 2))
  expect_error(check_matrix(x, ncol = 5))
  expect_error(check_matrix(x, nrow = at_least(4)))
  expect_error(check_matrix(x, ncol = at_most(3)))
  expect_error(check_matrix(x, nrow = in_range(4, 5)))
})

test_that("allow_null works correctly", {
  expect_null(check_array(NULL, allow_null = TRUE))
  expect_error(check_array(NULL, allow_null = FALSE))

  expect_null(check_matrix(NULL, allow_null = TRUE))
  expect_error(check_matrix(NULL, allow_null = FALSE))

  expect_snapshot(error = TRUE, {
    check_array(NULL, allow_null = FALSE)
    check_matrix(NULL, allow_null = FALSE)
  })
})

test_that("error shows type problem preferentially", {
  expect_snapshot(error = TRUE, {
    check_array(1, n = 2)
    check_matrix(1:2, n = 1)
    x <- array(1)
    class(x) <- c("my_array", class(x))
    check_array(bare(x), n = 2)
    x <- matrix(1)
    class(x) <- c("my_matrix", class(x))
    check_matrix(bare(x), n = 2)
  })
})

test_that("error shows length problem when types match", {
  expect_snapshot(error = TRUE, {
    check_array(array(1:2), n = 1)
    check_matrix(matrix(1:2), ncol = 2)
  })
})

test_that("arg is shown in error", {
  expect_snapshot(error = TRUE, {
    x <- quote(a + b)
    check_array(x)
    check_matrix(x)
    check_array(x, arg = "my_arg")
    check_matrix(x, arg = "my_arg")
  })
})

test_that("call is shown in error", {
  expect_snapshot(error = TRUE, {
    f <- function() {
      check_array(mean)
    }
    fs <- function() {
      check_matrix(mean)
    }
    f()
    fs()
  })
})

test_that("dots passed to cli_abort/abort", {
  expect_snapshot(error = TRUE, {
    check_array(mean, footer = "Custom footer")
    check_matrix(mean, footer = "Custom footer")
  })
})

test_that(".envir doesn't interfere", {
  expect_snapshot(error = TRUE, {
    e <- environment()
    e$arg <- "my_arg"
    check_array(mean, .envir = e)
    check_matrix(mean, .envir = e)
  })
})
