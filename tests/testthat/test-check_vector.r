test_that("detects correct type", {
  for (type in bare_vector_types()) {
    expect_null(check_vector(type))
    expect_null(check_vector(bare(type)))
    expect_null(check_scalar_vector(type))
    expect_null(check_scalar_vector(bare(type)))
  }

  for (type in classed_vector_types()) {
    expect_null(check_vector(type))
    expect_error(check_vector(bare(type)))
    expect_null(check_scalar_vector(type))
    expect_error(check_scalar_vector(bare(type)))
  }

  for (na in na_atomics()) {
    expect_null(check_vector(na))
    expect_null(check_scalar_vector(na))
  }

  expect_error(check_vector(expression(1)))
  expect_error(check_vector(NULL))
  expect_error(check_vector(mean))

  expect_error(check_scalar_vector(expression(1)))
  expect_error(check_scalar_vector(NULL))
  expect_error(check_scalar_vector(mean))
})

test_that("n arg checks length correctly", {
  expect_null(check_vector(list(1, 2), n = 2))
  expect_error(check_vector(list(1, 2), n = 1))
  expect_error(check_vector(list(1, 2), n = 3))
  expect_null(check_vector(list(list(1, 2)), n = 1))

  expect_null(check_vector(list(list(1, 2)), n = at_least(1)))
  expect_error(check_vector(list(list(1, 2)), n = at_least(2)))
  expect_null(check_vector(list(list(1, 2)), n = at_most(1)))
  expect_error(check_vector(list(list(1, 2)), n = at_most(0)))
  expect_null(check_vector(list(1, 2), n = in_range(1, 3)))
  expect_error(check_vector(list(list(1, 2)), n = in_range(2, 3)))

  expect_null(check_scalar_vector(list(1)))
  expect_null(check_scalar_vector(list(list(1, 2))))
  expect_error(check_scalar_vector(list(1, 2)))
})

test_that("allow_null works correctly", {
  expect_null(check_vector(NULL, allow_null = TRUE))
  expect_error(check_vector(NULL, allow_null = FALSE))

  expect_null(check_scalar_vector(NULL, allow_null = TRUE))
  expect_error(check_scalar_vector(NULL, allow_null = FALSE))

  expect_snapshot(error = TRUE, {
    check_vector(NULL, allow_null = FALSE)
    check_scalar_vector(NULL, allow_null = FALSE)
  })
})

test_that("error shows type problem preferentially", {
  expect_snapshot(error = TRUE, {
    e <- new.env()
    e$x <- list(1)
    e$y <- 1
    check_vector(e, n = 2)
    check_vector(e, n = 1)
    check_vector(bare(structure(1, class = "my_class")), n = 2)
    check_scalar_vector(e)
    check_scalar_vector(bare(structure(c(1, 2), class = "my_class")))
  })
})

test_that("error shows length problem when types match", {
  expect_snapshot(error = TRUE, {
    check_vector(c(TRUE, FALSE), n = 1)
    check_scalar_vector(c(TRUE, FALSE))
  })
})

test_that("arg is shown in error", {
  expect_snapshot(error = TRUE, {
    x <- quote(a + b)
    check_vector(x)
    check_scalar_vector(x)
    check_vector(x, arg = "my_arg")
    check_scalar_vector(x, arg = "my_arg")
  })
})

test_that("call is shown in error", {
  expect_snapshot(error = TRUE, {
    f <- function() {
      check_vector(mean)
    }
    fs <- function() {
      check_scalar_vector(mean)
    }
    f()
    fs()
  })
})

test_that("dots passed to cli_abort/abort", {
  expect_snapshot(error = TRUE, {
    check_vector(mean, footer = "Custom footer")
    check_scalar_vector(mean, footer = "Custom footer")
  })
})

test_that(".envir doesn't interfere", {
  expect_snapshot(error = TRUE, {
    e <- environment()
    e$arg <- "my_arg"
    check_vector(mean, .envir = e)
    check_scalar_vector(mean, .envir = e)
  })
})
