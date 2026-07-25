test_that("detects correct type", {
  expect_null(check_string("a"))
  # default allows empty
  expect_null(check_string(""))
  expect_error(check_string(NA_character_))
  expect_error(check_string(1L))
  expect_error(check_string(TRUE))
  expect_error(check_string(NULL))
  expect_error(check_string(NA))
  expect_error(check_string(bare("a")))
})

test_that("NA not allowed", {
  expect_error(check_string(NA_character_))
  expect_error(check_string(NA))
})

test_that("allow_null arg works correctly", {
  expect_null(check_string(NULL, allow_null = TRUE))
  expect_error(check_string(NULL, allow_null = FALSE))
})

test_that("allow_empty arg works correctly", {
  expect_null(check_string("a", allow_empty = TRUE))
  expect_error(check_string("", allow_empty = FALSE))
  expect_null(check_string("", allow_empty = TRUE))

  expect_snapshot(error = TRUE, {
    check_string("", allow_empty = FALSE)
  })
})

test_that("string arg works correctly", {
  expect_null(check_string("a", string = "a"))
  expect_error(check_string("b", string = "a"))
  expect_null(check_string("z", string = letters))
  expect_error(check_string("z", string = letters[1:5]))

  expect_error(check_string("a", string = list("a", "b")))

  # NA_character_ allowed but would never match as `x` not
  # allowed to be NA_character_.
  expect_null(check_string("a1", string = c(NA_character_, "a1")))

  expect_snapshot(error = TRUE, {
    # singular
    check_string("b", string = "a")
    # pluralization with 'or'
    check_string("z", string = letters[1:5])
  })
})

test_that("error shows type problem preferentially", {
  expect_snapshot(error = TRUE, {
    check_string(1L)
    check_string(c(1L, 2L))
  })
})

test_that("error shows length problem when types match", {
  expect_snapshot(error = TRUE, {
    check_string(c("a", "b"))
  })
})

test_that("arg is shown in error", {
  expect_snapshot(error = TRUE, {
    x <- 1L
    check_string(x)
    check_string(x, arg = "my_arg")
  })
})

test_that("call is shown in error", {
  expect_snapshot(error = TRUE, {
    f <- function() {
      check_string(1L)
    }
    f()
  })
})

test_that("dots passed to cli_abort/abort", {
  expect_snapshot(error = TRUE, {
    check_string(1L, footer = "Custom footer")
  })
})

test_that(".envir doesn't interfere", {
  expect_snapshot(error = TRUE, {
    e <- environment()
    e$arg <- "my_arg"
    check_string(1L, .envir = e)
  })
})
