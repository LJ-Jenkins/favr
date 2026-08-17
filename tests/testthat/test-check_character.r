test_that("detects correct type", {
  expect_null(check_character(""))
  expect_null(check_character(bare("")))
  expect_null(check_character(NA_character_))
  expect_error(check_character(1L))
  expect_error(check_character(TRUE))
  expect_error(check_character(NULL))
  expect_error(check_character(NA))
  x <- structure("", class = "my_class")
  expect_error(check_character(bare(x)))

  expect_null(check_scalar_character(""))
  expect_null(check_scalar_character(bare("")))
  expect_null(check_scalar_character(NA_character_))
  expect_error(check_scalar_character(1L))
  expect_error(check_scalar_character(TRUE))
  expect_error(check_scalar_character(NULL))
  expect_error(check_scalar_character(NA))
  expect_error(check_scalar_character(bare(x)))
})

test_that("n arg checks length correctly", {
  expect_null(check_character(c("a", "b"), n = 2))
  expect_error(check_character(c("a", "b"), n = 1))
  expect_error(check_character(c("a", "b"), n = 3))

  expect_null(check_character(c("a", "b"), n = at_least(1)))
  expect_error(check_character(c("a", "b"), n = at_least(3)))
  expect_null(check_character(c("a", "b"), n = at_most(3)))
  expect_error(check_character(c("a", "b"), n = at_most(1)))
  expect_null(check_character(c("a", "b"), n = in_range(1, 3)))
  expect_error(check_character(c("a", "b"), n = in_range(3, 5)))

  expect_null(check_scalar_character("a"))
  expect_error(check_scalar_character(c("a", "b")))
})

test_that("allow_na and allow_null work correctly", {
  expect_null(check_character(c("a", "b", NA_character_), allow_na = TRUE))
  expect_error(check_character(c("a", "b", NA_character_), allow_na = FALSE))
  expect_null(check_character(NULL, allow_null = TRUE))
  expect_error(check_character(NULL, allow_null = FALSE))

  expect_null(check_scalar_character(NA_character_, allow_na = TRUE))
  expect_error(check_scalar_character(NA_character_, allow_na = FALSE))
  expect_null(check_scalar_character(NULL, allow_null = TRUE))
  expect_error(check_scalar_character(NULL, allow_null = FALSE))

  expect_snapshot(error = TRUE, {
    check_character(c("a", "b", NA_character_), allow_na = FALSE)
    check_character(NULL, allow_null = FALSE)
    check_scalar_character(NA_character_, allow_na = FALSE)
    check_scalar_character(NULL, allow_null = FALSE)
  })
})

test_that("error shows type problem preferentially", {
  expect_snapshot(error = TRUE, {
    check_character(1L, n = 2)
    check_character(bare(structure("", class = "my_class")), n = 2)
    check_character(c(1L, 2L), n = 1)
    check_scalar_character(c(1L, 2L))
    check_scalar_character(bare(structure(c("a", "b"), class = "my_class")))
  })
})

test_that("error shows length problem when types match", {
  expect_snapshot(error = TRUE, {
    check_character(c("a", "b"), n = 1)
    check_scalar_character(c("a", "b"))
  })
})

test_that("arg is shown in error", {
  expect_snapshot(error = TRUE, {
    x <- 1L
    check_character(x)
    check_scalar_character(x)
    check_character(x, n = 2, arg = "my_arg")
    check_scalar_character(x, arg = "my_arg")
  })
})

test_that("call is shown in error", {
  expect_snapshot(error = TRUE, {
    f <- function() {
      check_character(1L)
    }
    fs <- function() {
      check_scalar_character(1L)
    }
    f()
    fs()
  })
})

test_that("dots passed to cli_abort/abort", {
  expect_snapshot(error = TRUE, {
    check_character(1L, footer = "Custom footer")
    check_scalar_character(1L, footer = "Custom footer")
  })
})

test_that(".envir doesn't interfere", {
  expect_snapshot(error = TRUE, {
    e <- environment()
    e$arg <- "my_arg"
    check_character(1L, .envir = e)
    check_scalar_character(1L, .envir = e)
  })
})
