test_that("detects oop objects", {
  expect_null(check_s3(factor("a")))
  expect_error(check_s3(1:3))

  methods::setClass("Person",
    slots = c(name = "character", age = "numeric")
  )
  x <- methods::new("Person", name = "John", age = 30)
  expect_null(check_s4(x))
  expect_error(check_s4(factor("a")))

  x <- structure(list(), class = "S7_object")
  expect_null(check_s7(x))
  expect_error(check_s7(factor("a")))

  x <- structure(list(), class = "R6")
  expect_null(check_r6(x))
  expect_error(check_r6(factor("a")))
})

test_that("allow_null arg works correctly", {
  expect_null(check_s3(NULL, allow_null = TRUE))
  expect_error(check_s3(NULL, allow_null = FALSE))

  expect_null(check_s4(NULL, allow_null = TRUE))
  expect_error(check_s4(NULL, allow_null = FALSE))

  expect_null(check_s7(NULL, allow_null = TRUE))
  expect_error(check_s7(NULL, allow_null = FALSE))

  expect_null(check_r6(NULL, allow_null = TRUE))
  expect_error(check_r6(NULL, allow_null = FALSE))
})

test_that("arg is shown in error", {
  expect_snapshot(error = TRUE, {
    x <- 1:2
    check_s3(x)
    check_s3(x, arg = "my_arg")
    check_s4(x)
    check_s4(x, arg = "my_arg")
    check_s7(x)
    check_s7(x, arg = "my_arg")
    check_r6(x)
    check_r6(x, arg = "my_arg")
  })
})

test_that("call is shown in error", {
  expect_snapshot(error = TRUE, {
    f <- function() {
      check_s3(1)
    }
    f()

    f <- function() {
      check_s4(1)
    }
    f()

    f <- function() {
      check_s7(1)
    }
    f()

    f <- function() {
      check_r6(1)
    }
    f()
  })
})

test_that("dots passed to cli_abort/abort", {
  expect_snapshot(error = TRUE, {
    check_s3(1, footer = "Custom footer")
    check_s4(1, footer = "Custom footer")
    check_s7(1, footer = "Custom footer")
    check_r6(1, footer = "Custom footer")
  })
})

test_that(".envir doesn't interfere", {
  expect_snapshot(error = TRUE, {
    e <- environment()
    e$arg <- "my_arg"
    check_s3(1, .envir = e)
    check_s4(1, .envir = e)
    check_s7(1, .envir = e)
    check_r6(1, .envir = e)
  })
})
