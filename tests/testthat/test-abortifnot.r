test_that("abortifnot no error for TRUE", {
  expect_no_error(abortifnot(TRUE))
  expect_no_error(abortifnot(c(TRUE, TRUE)))
  x <- 1:10
  expect_no_error(abortifnot(is.integer(x)))
  expect_no_error(abortifnot(is.integer(x), is.numeric(x)))
  expect_no_error(abortifnot(quote(TRUE)))
})

test_that("abortifnot error for FALSE", {
  expect_error(abortifnot(FALSE))
  expect_error(abortifnot(c(TRUE, FALSE)))
  x <- 1:10
  expect_error(abortifnot(!is.integer(x)))
  expect_error(abortifnot(is.integer(x), !is.numeric(x)))
  expect_error(abortifnot(!is.integer(x), is.numeric(x)))
  expect_error(abortifnot(quote(FALSE)))
})

test_that("abortifnot error for NA", {
  x <- c(1, 2, NA, 10)
  expect_error(abortifnot(NA))
  expect_error(abortifnot(c(TRUE, NA)))
  expect_error(abortifnot(x > 0))
})

test_that("abortifnot error when arg isn't logical", {
  expect_error(abortifnot(10))
  expect_error(abortifnot(NULL))
  expect_error(abortifnot(list(TRUE)))
  expect_error(abortifnot(data.frame(x = TRUE)))
  expect_error(abortifnot(\(x) TRUE))
})

test_that("abortifnot error message includes argument name", {
  expect_error(abortifnot(FALSE), "FALSE")
  expect_error(abortifnot(c(TRUE, FALSE)), "(TRUE, FALSE)")
  x <- 1:10
  expect_error(abortifnot(is.double(x)), "is.double")
  expect_error(abortifnot(is.double(x), !is.numeric(x)), "is.double")
  expect_error(abortifnot(!is.double(x), !is.numeric(x)), "!is.numeric")
  expect_error(abortifnot(quote(FALSE)), "quote")
})

test_that("abortifnot error message indicates issue", {
  expect_snapshot(error = TRUE, {
    # wrong type singular
    abortifnot(1)
    # wrong type vector
    abortifnot(1:5)
    # NA values
    abortifnot(c(TRUE, NA))
    # singular NA
    abortifnot(NA)
    # not TRUE
    abortifnot(FALSE)
  })
})

test_that("abortifnot error message includes custom default message", {
  expect_error(abortifnot(FALSE, message = "Custom message"), "Custom message")
  expect_snapshot(error = TRUE, {
    x <- 1:3
    abortifnot(
      is.character(x),
      message = "{.pkg cli} formatted info: {.val {x}}"
    )
  })
})

test_that("abortifnot error mesage derived from names", {
  expect_error(abortifnot("message for x" = 1 != 1), "message for x")
  expect_error(
    abortifnot("message for x" = 1 != 1, message = "overriden"),
    "message for x"
  )

  expect_snapshot(error = TRUE, {
    x <- 1:3
    abortifnot("{.pkg cli} message for {.val {x}}" = is.character(x))
  })
})

test_that("abortifnot doesn't test abort_args unless error", {
  expect_null(abortifnot(TRUE, abort_args = "not a list"))
})

test_that("abortifnot shows error about abort_args if error occurring", {
  expect_error(abortifnot(1, abort_args = "not a list"))

  expect_snapshot(error = TRUE, {
    abortifnot(1, abort_args = "not a list")
  })
})

test_that("abortifnot passes abort_args", {
  expect_snapshot(error = TRUE, {
    x <- 1:3
    abortifnot(is.character(x), abort_args = list(footer = "custom footer"))
  })
})

test_that("abortifnot default errors display properly no matter the .envir", {
  expect_snapshot(error = TRUE, {
    e <- new.env()
    e$arg <- "Shouldn't show"
    abortifnot(1, .envir = e)
  })
})

test_that("abortifnot custom name/message errors use .envir", {
  expect_snapshot(error = TRUE, {
    e <- new.env()
    e$arg <- "Should show"
    abortifnot("custom message: {.arg {arg}}" = 1, .envir = e)
    abortifnot(1, message = "custom message: {.arg {arg}}", .envir = e)
  })
})
