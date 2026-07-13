test_that("abortif no error for FALSE", {
  expect_no_error(abortif(FALSE))
  expect_no_error(abortif(c(FALSE, FALSE)))
  x <- 1:10
  expect_no_error(abortif(is.double(x)))
  expect_no_error(abortif(is.character(x), is.list(x)))
  expect_no_error(abortif(quote(FALSE)))
})

test_that("abortif error for TRUE", {
  expect_error(abortif(TRUE))
  expect_error(abortif(c(TRUE, FALSE)))
  x <- 1:10
  expect_error(abortif(is.integer(x)))
  expect_error(abortif(is.integer(x), !is.numeric(x)))
  expect_error(abortif(!is.integer(x), is.numeric(x)))
  expect_error(abortif(quote(TRUE)))
})

test_that("abortif error for NA", {
  x <- c(1, 2, NA, 10)
  expect_error(abortif(NA))
  expect_error(abortif(c(FALSE, NA)))
  expect_error(abortif(x > 0))
})

test_that("abortif error when arg isn't logical", {
  expect_error(abortif(10))
  expect_error(abortif(NULL))
  expect_error(abortif(list(TRUE)))
  expect_error(abortif(data.frame(x = TRUE)))
  expect_error(abortif(\(x) TRUE))
})

test_that("abortif error message includes argument name", {
  expect_error(abortif(TRUE), "TRUE")
  expect_error(abortif(c(TRUE, FALSE)), "(TRUE, FALSE)")
  x <- 1:10
  expect_error(abortif(is.integer(x)), "is.integer")
  expect_error(abortif(is.integer(x), !is.numeric(x)), "is.integer")
  expect_error(abortif(!is.integer(x), is.numeric(x)), "is.numeric")
  expect_error(abortif(quote(TRUE)), "quote")
})

test_that("abortif error message indicates issue", {
  expect_snapshot(error = TRUE, {
    # wrong type singular
    abortif(1)
    # wrong type vector
    abortif(1:5)
    # NA values
    abortif(c(FALSE, NA))
    # singular NA
    abortif(NA)
    # is TRUE
    abortif(TRUE)
  })
})

test_that("abortif error message includes custom default message", {
  expect_error(abortif(TRUE, message = "Custom message"), "Custom message")
  expect_snapshot(error = TRUE, {
    x <- 1:3
    abortif(
      !is.character(x),
      message = "{.pkg cli} formatted info: {.val {x}}"
    )
  })
})

test_that("abortif error mesage derived from names", {
  expect_error(abortif("message for x" = 1 == 1), "message for x")
  expect_error(
    abortif("message for x" = 1 == 1, message = "overriden"),
    "message for x"
  )

  expect_snapshot(error = TRUE, {
    x <- 1:3
    abortif("{.pkg cli} message for {.val {x}}" = !is.character(x))
  })
})

test_that("abortif doesn't test abort_args unless error", {
  expect_null(abortif(FALSE, abort_args = "not a list"))
})

test_that("abortif shows error about abort_args if error occurring", {
  expect_error(abortif(1, abort_args = "not a list"))

  expect_snapshot(error = TRUE, {
    abortifnot(1, abort_args = "not a list")
  })
})

test_that("abortif passes abort_args", {
  expect_snapshot(error = TRUE, {
    x <- 1:3
    abortif(!is.character(x), abort_args = list(footer = "custom footer"))
  })
})

test_that("abortif default errors display properly no matter the .envir", {
  expect_snapshot(error = TRUE, {
    e <- new.env()
    e$arg <- "Shouldn't show"
    abortif(1, .envir = e)
  })
})

test_that("abortif custom name/message errors use .envir", {
  expect_snapshot(error = TRUE, {
    e <- new.env()
    e$arg <- "Should show"
    abortif("custom message: {.arg {arg}}" = 1, .envir = e)
    abortif(1, message = "custom message: {.arg {arg}}", .envir = e)
  })
})
