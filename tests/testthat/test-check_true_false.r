test_that("detects correct type/size", {
  expect_null(check_true(TRUE))
  expect_error(check_true(1L))
  expect_error(check_true(FALSE))
  expect_error(check_true(NA))
  expect_error(check_true(c(TRUE, TRUE)))
  expect_error(check_true(bare(TRUE)))

  expect_null(check_false(FALSE))
  expect_error(check_false(1L))
  expect_error(check_false(TRUE))
  expect_error(check_false(NA))
  expect_error(check_false(c(FALSE, FALSE)))
  expect_error(check_false(bare(FALSE)))
})

test_that("allow_null arg works correctly", {
  expect_null(check_true(NULL, allow_null = TRUE))
  expect_error(check_true(NULL, allow_null = FALSE))

  expect_null(check_false(NULL, allow_null = TRUE))
  expect_error(check_false(NULL, allow_null = FALSE))
})

test_that("error shows type problem", {
  expect_snapshot(error = TRUE, {
    check_true(1L)
    check_true(c(1L, 2L))
    check_false(1L)
    check_false(c(1L, 2L))
  })
})

test_that("arg is shown in error", {
  expect_snapshot(error = TRUE, {
    x <- 1L
    check_true(x)
    check_true(x, arg = "my_arg")
    check_false(x)
    check_false(x, arg = "my_arg")
  })
})

test_that("call is shown in error", {
  expect_snapshot(error = TRUE, {
    f <- function() {
      check_true(1L)
    }
    f()

    f <- function() {
      check_false(1L)
    }
    f()
  })
})

test_that("dots evaluated and passed to cli_abort/abort", {
  expect_snapshot(error = TRUE, {
    check_true(1L, footer = "Custom footer")
    check_false(1L, footer = "Custom footer")
  })
})

test_that(".envir is silently overriden", {
  expect_snapshot(error = TRUE, {
    e <- environment()
    e$arg <- "my_arg"
    check_true(1L, .envir = e)
    check_false(1L, .envir = e)
  })
})
