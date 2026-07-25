test_that("detects correct type", {
  for (type in bare_atomic_types()) {
    expect_null(check_atomic(type))
    expect_null(check_atomic(bare(type)))
    expect_null(check_scalar_atomic(type))
    expect_null(check_scalar_atomic(bare(type)))
  }

  for (type in classed_atomic_types()) {
    expect_null(check_atomic(type))
    expect_error(check_atomic(bare(type)))
    expect_null(check_scalar_atomic(type))
    expect_error(check_scalar_atomic(bare(type)))
  }

  for (na in na_atomics()) {
    expect_null(check_atomic(na))
    expect_null(check_scalar_atomic(na))
  }


  expect_error(check_atomic(list(1)))
  expect_error(check_atomic(data.frame(x = 1)))
  expect_error(check_atomic(NULL))
  expect_error(check_atomic(mean))


  expect_error(check_scalar_atomic(list(1)))
  expect_error(check_scalar_atomic(data.frame(x = 1)))
  expect_error(check_scalar_atomic(NULL))
  expect_error(check_scalar_atomic(mean))
})

test_that("n arg checks length correctly", {
  expect_null(check_atomic(c(TRUE, FALSE), n = 2))
  expect_error(check_atomic(c(TRUE, FALSE), n = 1))
  expect_error(check_atomic(c(TRUE, FALSE), n = 3))

  expect_null(check_atomic(c(TRUE, FALSE), n = at_least(1)))
  expect_error(check_atomic(c(TRUE, FALSE), n = at_least(3)))
  expect_null(check_atomic(c(TRUE, FALSE), n = at_most(3)))
  expect_error(check_atomic(c(TRUE, FALSE), n = at_most(1)))
  expect_null(check_atomic(c(TRUE, FALSE), n = in_range(1, 3)))
  expect_error(check_atomic(c(TRUE, FALSE), n = in_range(3, 5)))

  expect_null(check_scalar_atomic(TRUE))
  expect_error(check_scalar_atomic(c(TRUE, FALSE)))
})

test_that("allow_na and allow_null works correctly", {
  expect_null(check_atomic(c(TRUE, FALSE, NA), allow_na = TRUE))
  expect_error(check_atomic(c(TRUE, FALSE, NA), allow_na = FALSE))
  expect_null(check_atomic(NULL, allow_null = TRUE))
  expect_error(check_atomic(NULL, allow_null = FALSE))

  expect_null(check_scalar_atomic(NA, allow_na = TRUE))
  expect_error(check_scalar_atomic(NA, allow_na = FALSE))
  expect_null(check_scalar_atomic(NULL, allow_null = TRUE))
  expect_error(check_scalar_atomic(NULL, allow_null = FALSE))

  expect_snapshot(error = TRUE, {
    check_atomic(c(TRUE, FALSE, NA), allow_na = FALSE)
    check_atomic(NULL, allow_null = FALSE)
    check_scalar_atomic(NA, allow_na = FALSE)
    check_scalar_atomic(NULL, allow_null = FALSE)
  })
})

test_that("error shows type problem preferentially", {
  expect_snapshot(error = TRUE, {
    check_atomic(list(1), n = 2)
    check_atomic(bare(structure(1, class = "my_class")), n = 2)
    check_atomic(list(1, 2), n = 1)
    check_scalar_atomic(list(1, 2))
    check_scalar_atomic(bare(structure(c(TRUE, FALSE), class = "my_class")))
  })
})

test_that("error shows length problem when types match", {
  expect_snapshot(error = TRUE, {
    check_atomic(c(TRUE, FALSE), n = 1)
    check_scalar_atomic(c(TRUE, FALSE))
  })
})

test_that("arg is shown in error", {
  expect_snapshot(error = TRUE, {
    x <- list(1)
    check_atomic(x)
    check_scalar_atomic(x)
    check_atomic(x, arg = "my_arg")
    check_scalar_atomic(x, arg = "my_arg")
  })
})

test_that("call is shown in error", {
  expect_snapshot(error = TRUE, {
    f <- function() {
      check_atomic(list(1))
    }
    fs <- function() {
      check_scalar_atomic(list(1))
    }
    f()
    fs()
  })
})

test_that("dots passed to cli_abort/abort", {
  expect_snapshot(error = TRUE, {
    check_atomic(list(1), footer = "Custom footer")
    check_scalar_atomic(list(1), footer = "Custom footer")
  })
})

test_that(".envir doesn't interfere", {
  expect_snapshot(error = TRUE, {
    e <- environment()
    e$arg <- "my_arg"
    check_atomic(list(1), .envir = e)
    check_scalar_atomic(list(1), .envir = e)
  })
})
