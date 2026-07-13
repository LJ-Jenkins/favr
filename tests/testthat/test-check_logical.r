test_that("detects correct type", {
  expect_null(check_logical(TRUE))
  expect_null(check_logical(bare(FALSE)))
  expect_null(check_logical(NA))
  expect_error(check_logical(1))
  expect_error(check_logical("TRUE"))
  expect_error(check_logical(NULL))
  expect_error(check_logical(NA_character_))
  x <- structure(TRUE, class = "my_class")
  expect_error(check_logical(bare(x)))

  expect_null(check_scalar_logical(TRUE))
  expect_null(check_scalar_logical(bare(FALSE)))
  expect_null(check_scalar_logical(NA))
  expect_error(check_scalar_logical(1))
  expect_error(check_scalar_logical("TRUE"))
  expect_error(check_scalar_logical(NULL))
  expect_error(check_scalar_logical(NA_character_))
  expect_error(check_scalar_logical(bare(x)))
})

test_that("n arg checks length correctly", {
  expect_null(check_logical(c(TRUE, FALSE), n = 2))
  expect_error(check_logical(c(TRUE, FALSE), n = 1))
  expect_error(check_logical(c(TRUE, FALSE), n = 3))

  expect_null(check_logical(c(TRUE, FALSE), n = at_least(1)))
  expect_error(check_logical(c(TRUE, FALSE), n = at_least(3)))
  expect_null(check_logical(c(TRUE, FALSE), n = at_most(3)))
  expect_error(check_logical(c(TRUE, FALSE), n = at_most(1)))
  expect_null(check_logical(c(TRUE, FALSE), n = in_range(1, 3)))
  expect_error(check_logical(c(TRUE, FALSE), n = in_range(3, 5)))

  expect_null(check_scalar_logical(TRUE))
  expect_error(check_scalar_logical(c(TRUE, FALSE)))
})

test_that("allow_na and allow_null work correctly", {
  expect_null(check_logical(c(TRUE, FALSE, NA), allow_na = TRUE))
  expect_error(check_logical(c(TRUE, FALSE, NA), allow_na = FALSE))
  expect_null(check_logical(NULL, allow_null = TRUE))
  expect_error(check_logical(NULL, allow_null = FALSE))

  expect_null(check_scalar_logical(NA, allow_na = TRUE))
  expect_error(check_scalar_logical(NA, allow_na = FALSE))
  expect_null(check_scalar_logical(NULL, allow_null = TRUE))
  expect_error(check_scalar_logical(NULL, allow_null = FALSE))

  expect_snapshot(error = TRUE, {
    check_logical(c(TRUE, FALSE, NA), allow_na = FALSE)
    check_logical(NULL, allow_null = FALSE)
    check_scalar_logical(NA, allow_na = FALSE)
    check_scalar_logical(NULL, allow_null = FALSE)
  })
})

test_that("error shows type problem preferentially", {
  expect_snapshot(error = TRUE, {
    check_logical(1, n = 2)
    check_logical(1:2, n = 1)
    check_logical(structure(TRUE, class = "my_class"), n = 2)
    check_scalar_logical(1:2)
    check_scalar_logical(structure(c(TRUE, FALSE), class = "my_class"))
  })
})

test_that("error shows length problem when types match", {
  expect_snapshot(error = TRUE, {
    check_logical(c(TRUE, FALSE), n = 1)
    check_scalar_logical(c(TRUE, FALSE))
  })
})

test_that("arg is shown in error", {
  expect_snapshot(error = TRUE, {
    x <- 1
    check_logical(x)
    check_scalar_logical(x)
    check_logical(x, arg = "my_arg")
    check_scalar_logical(x, arg = "my_arg")
  })
})

test_that("call is shown in error", {
  expect_snapshot(error = TRUE, {
    f <- function() {
      check_logical(1)
    }
    fs <- function() {
      check_scalar_logical(1)
    }
    f()
    fs()
  })
})

test_that("dots evaluated and passed to cli_abort/abort", {
  expect_snapshot(error = TRUE, {
    check_logical(1, footer = "Custom footer")
    check_scalar_logical(1, footer = "Custom footer")
  })
})

test_that(".envir is silently overriden", {
  expect_snapshot(error = TRUE, {
    e <- environment()
    e$arg <- "my_arg"
    check_logical(1, .envir = e)
    check_scalar_logical(1, .envir = e)
  })
})
