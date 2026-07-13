test_that("check no error for TRUE", {
  expect_no_error(check(TRUE))
  expect_no_error(check(c(TRUE, TRUE)))
  x <- 1:10
  expect_no_error(check(is.integer(x)))
  expect_no_error(check(is.integer(x), is.numeric(x)))
  expect_no_error(check(quote(TRUE)))
})

test_that("check error for FALSE", {
  expect_error(check(FALSE))
  expect_error(check(c(TRUE, FALSE)))
  x <- 1:10
  expect_error(check(!is.integer(x)))
  expect_error(check(is.integer(x), !is.numeric(x)))
  expect_error(check(!is.integer(x), is.numeric(x)))
  expect_error(check(quote(FALSE)))
})

test_that("check error for NA", {
  x <- c(1, 2, NA, 10)
  expect_error(check(NA))
  expect_error(check(c(TRUE, NA)))
  expect_error(check(x > 0))
})

test_that("check error when arg isn't logical", {
  expect_error(check(10))
  expect_error(check(NULL))
  expect_error(check(list(TRUE)))
  expect_error(check(data.frame(x = TRUE)))
  expect_error(check(\(x) TRUE))
})

test_that("check error message includes argument name", {
  expect_error(check(1 != 1), "1 != 1")
  expect_error(check(c(TRUE, FALSE)), "(TRUE, FALSE)")
  x <- 1:10
  expect_error(check(is.double(x)), "is.double")
  expect_error(check(is.double(x), !is.numeric(x)), "is.double")
  expect_error(check(!is.double(x), !is.numeric(x)), "!is.numeric")
  expect_error(check(quote(FALSE)), "quote")
})

test_that("check error message includes custom default message", {
  expect_error(check(1 != 1, message = "Custom message"), "Custom message")
  expect_snapshot(error = TRUE, {
    x <- 1:3
    check(
      is.character(x),
      message = "{.pkg cli} formatted info: {.val {x}}"
    )
  })
})

test_that("check error mesage derived from names", {
  expect_error(check("message for x" = 1 != 1), "message for x")
  expect_error(
    check("message for x" = 1 != 1, message = "overriden"),
    "message for x"
  )

  expect_snapshot(error = TRUE, {
    x <- 1:3
    check("{.pkg cli} message for {.val {x}}" = is.character(x))
  })
})

test_that("check doesn't test abort_args unless error", {
  expect_null(check(TRUE, abort_args = "not a list"))
})

test_that("check shows error about abort_args if error occurring", {
  expect_error(check(1, abort_args = "not a list"))

  expect_snapshot(error = TRUE, {
    check(1, abort_args = "not a list")
  })
})

test_that("check passes abort_args", {
  expect_snapshot(error = TRUE, {
    x <- 1:3
    check(is.character(x), abort_args = list(footer = "custom footer"))
  })
})

test_that("check default errors display properly no matter the .envir", {
  expect_snapshot(error = TRUE, {
    e <- new.env()
    e$arg <- "Shouldn't show"
    check(1, .envir = e)
  })
})

test_that("check custom name/message errors use .envir", {
  expect_snapshot(error = TRUE, {
    e <- new.env()
    e$arg <- "Should show"
    check("custom message: {.arg {arg}}" = 1, .envir = e)
    check(1, message = "custom message: {.arg {arg}}", .envir = e)
  })
})

test_that("check works with injection", {
  x <- 1:3
  y <- rlang::sym("x")
  expect_null(check(is.numeric(!!y)))
  expect_error(check(is.character(!!y)))

  msg <- "my message"
  expect_error(check(!!msg := FALSE), "my message")

  x <- list(a = TRUE, "my_message" = FALSE)
  expect_error(check(!!!x), "my_message")
})
