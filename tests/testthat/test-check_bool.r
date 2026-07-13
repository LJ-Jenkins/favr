test_that("detects correct type/size", {
  expect_null(check_bool(TRUE))
  expect_null(check_bool(FALSE))
  expect_error(check_bool(1L))
  expect_error(check_bool(NA))
  expect_error(check_bool(c(TRUE, TRUE)))
  expect_error(check_bool(bare(TRUE)))
})

test_that("allow_null arg works correctly", {
  expect_null(check_bool(NULL, allow_null = TRUE))
  expect_error(check_bool(NULL, allow_null = FALSE))
})

test_that("error shows type problem", {
  expect_snapshot(error = TRUE, {
    check_bool(1L)
    check_bool(c(1L, 2L))
  })
})

test_that("arg is shown in error", {
  expect_snapshot(error = TRUE, {
    x <- 1L
    check_bool(x)
    check_bool(x, arg = "my_arg")
  })
})

test_that("call is shown in error", {
  expect_snapshot(error = TRUE, {
    f <- function() {
      check_bool(1L)
    }
    f()
  })
})

test_that("dots evaluated and passed to cli_abort/abort", {
  expect_snapshot(error = TRUE, {
    check_bool(1L, footer = "Custom footer")
  })
})

test_that(".envir is silently overriden", {
  expect_snapshot(error = TRUE, {
    e <- environment()
    e$arg <- "my_arg"
    check_bool(1L, .envir = e)
  })
})
