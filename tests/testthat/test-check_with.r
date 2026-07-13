test_that("check_with .data must follow eval_tidy semantics", {
  # A data frame, or named list or vector. Alternatively, a data mask
  # created with as_data_mask() or new_data_mask(). Objects in data have
  # priority over those in env.

  # no names
  expect_error(check_with(1:10, x > 0))
  expect_error(check_with(list(1, 2, 3), x > 0))

  # non vector
  expect_error(check_with(mean, x > 0))
})

test_that("check_with errors if cannot find variable in .data or .envir", {
  expect_error(check_with(list(y = 1), x > 0))
  expect_error(check_with(list(x = 1), y > 0))
  expect_error(check_with(list(a = 1), x > 0, y > 0))
})

test_that("check_with prioritises .data over env", {
  x <- 0
  d <- list(x = 1)
  expect_identical(check_with(d, x > 0), d)
  expect_error(check_with(d, x == 0))
})

test_that("check_with chooses env if not found in .data", {
  x <- 0
  d <- list(y = 1)
  expect_identical(check_with(d, x == 0), d)
  expect_error(check_with(d, x > 0))
})

test_that("check_with works with pronouns", {
  x <- 0
  d <- list(x = 1)
  expect_identical(check_with(d, .data$x > 0), d)
  expect_error(check_with(d, .data$x == 0))
  expect_identical(check_with(d, .data[["x"]] > 0), d)
  expect_error(check_with(d, .data[["x"]] == 0))

  expect_identical(check_with(d, .env$x == 0), d)
  expect_error(check_with(d, .env$x > 0))
  expect_identical(check_with(d, .env[["x"]] == 0), d)
  expect_error(check_with(d, .env[["x"]] > 0))

  # incorrect usage errors
  expect_error(check_with(d, .data["x"] > 0))
  expect_error(check_with(d, .data[[x]] > 0))
})

test_that("check_with no error for TRUE", {
  expect_no_error(check_with(list(x = TRUE), x))
  expect_no_error(check_with(list(x = c(TRUE, TRUE)), x))
  x <- 1:10
  expect_no_error(check_with(list(x = x), is.integer(x)))
  expect_no_error(check_with(list(x = x), is.integer(x), is.numeric(x)))
  expect_no_error(check_with(list(x = TRUE), quote(TRUE)))
})

test_that("check_with error for FALSE", {
  expect_error(check_with(list(x = FALSE), x))
  expect_error(check_with(list(x = c(TRUE, FALSE)), x))
  x <- 1:10
  expect_error(check_with(list(x = x), !is.integer(x)))
  expect_error(check_with(list(x = x), is.integer(x), !is.numeric(x)))
  expect_error(check_with(list(x = x), !is.integer(x), is.numeric(x)))
  expect_error(check_with(list(x = FALSE), x, quote(FALSE)))
})

test_that("check_with error for NA", {
  x <- c(1, 2, NA, 10)
  expect_error(check_with(list(x = x), x > 0))
  expect_error(check_with(list(x = c(TRUE, NA)), x))
  expect_error(check_with(list(x = x), x > 0))
})

test_that("check_with error when arg isn't logical", {
  expect_error(check_with(list(x = 10), x))
  expect_error(check_with(list(x = NULL), x))
  expect_error(check_with(list(x = list(TRUE)), x))
  expect_error(check_with(list(x = data.frame(x = TRUE)), x))
  expect_error(check_with(list(x = \(x) TRUE), x))
})

test_that("check_with error message includes argument name", {
  expect_error(check_with(list(x = 1), x != 1), "x != 1")
  expect_error(check_with(list(x = c(TRUE, FALSE)), x), "x")
  x <- 1:10
  expect_error(check_with(list(x = x), is.double(x)), "is.double")
  expect_error(
    check_with(list(x = x), is.double(x), !is.numeric(x)),
    "is.double"
  )
  expect_error(
    check_with(list(x = x), !is.double(x), !is.numeric(x)),
    "!is.numeric"
  )
})

test_that("check_with error message includes custom default message", {
  expect_error(
    check_with(list(x = 1), x != 1, message = "Custom message"),
    "Custom message"
  )
  expect_snapshot(error = TRUE, {
    x <- "shouldn't show"
    check_with(
      list(x = 1:3),
      is.character(x),
      message = "{.pkg cli} formatted info: {.val {x}}"
    )
  })
})

test_that("check_with error mesage derived from names", {
  expect_error(
    check_with(list(x = 1), "message for x" = x != 1),
    "message for x"
  )
  expect_error(
    check_with(
      list(x = 1),
      "message for x" = x != 1,
      message = "overriden"
    ),
    "message for x"
  )
})

test_that("check_with error messages eval in .data then .envir", {
  x <- "+"
  y <- list(x = 1)
  expect_error(check_with(y, "{x}" = is.character(x)), "1")

  expect_snapshot(error = TRUE, {
    x <- "a"
    y <- list(x = 1:3)
    check_with(y, "{.pkg cli} message for {.val {x}}" = is.character(x))
  })

  b <- "+"
  y <- list(x = 1)
  expect_error(check_with(y, "{b}" = is.character(x)), "+")

  expect_snapshot(error = TRUE, {
    b <- "+"
    y <- list(x = 1)
    check_with(y, "{b}" = is.character(x))
  })

  x <- "+"
  e <- new.env()
  e$x <- "$"
  y <- list(x = 1)
  expect_error(check_with(y, "{x}" = is.character(x), .envir = e), "1")

  b <- "+"
  e <- new.env()
  e$b <- "$"
  y <- list(x = 1)
  expect_error(check_with(y, "{b}" = is.character(x), .envir = e), "$")

  expect_snapshot(error = TRUE, {
    b <- "+"
    e <- new.env()
    e$b <- "$"
    y <- list(x = 1)
    check_with(y, "{b}" = is.character(x), .envir = e)
  })
})

test_that("check_with error messages support pronouns", {
  # .data redundant as .data priortised anyway, but included for testing
  x <- "+"
  y <- list(x = 1)
  expect_error(check_with(y, "{ .data$x}" = is.character(x)), "1")

  expect_snapshot(error = TRUE, {
    x <- "a"
    y <- list(x = 1:3)
    check_with(y, "{ .data$x}" = is.character(x))
  })

  x <- "+"
  y <- list(x = 1)
  expect_error(check_with(y, "{ .env$x}" = is.character(x)), "+")

  expect_snapshot(error = TRUE, {
    x <- "+"
    y <- list(x = 1)
    check_with(y, "{ .env$x}" = is.character(x))
  })
})

test_that("check_with doesn't test abort_args unless error", {
  d <- list(x = TRUE)
  expect_identical(check_with(d, x, abort_args = "not a list"), d)
})

test_that("check_with shows error about abort_args if error occurring", {
  expect_error(check_with(list(x = 1), x, abort_args = "not a list"))

  expect_snapshot(error = TRUE, {
    check_with(list(x = 1), x, abort_args = "not a list")
  })
})

test_that("check_with passes abort_args", {
  expect_snapshot(error = TRUE, {
    x <- 1:3
    check_with(
      list(x = x),
      is.character(x),
      abort_args = list(footer = "custom footer")
    )
  })
})

test_that("check_with default errors display properly no matter the .envir", {
  expect_snapshot(error = TRUE, {
    e <- new.env()
    e$arg <- "Shouldn't show"
    check_with(list(x = 1), x, .envir = e)
  })
})

test_that("check_with custom name/message errors use .envir", {
  expect_snapshot(error = TRUE, {
    e <- new.env()
    e$arg <- "Should show"
    check_with("custom message: {.arg {arg}}" = 1, .envir = e)
    check_with(
      list(x = 1),
      x,
      message = "custom message: {.arg {arg}}",
      .envir = e
    )
  })
})

test_that("check_with works with injection", {
  v <- list(x = 1:3)
  y <- rlang::sym("x")
  expect_identical(check_with(v, is.numeric(!!y)), v)
  expect_error(check_with(v, is.character(!!y)))

  msg <- "my message"
  expect_error(check_with(v, !!msg := is.character(x)), "my message")

  x <- list(a = TRUE, "my_message" = FALSE)
  expect_error(check_with(v, !!!x), "my_message")
})

test_that("check_with works with `{{` defusing", {
  x <- 2
  y <- 1
  df <- data.frame(x = 1, y = 2)

  f <- \(data, var1, var2) {
    check_with(data, {{ var1 }} < {{ var2 }})
  }

  expect_identical(f(df, x, y), df)

  expect_error(f(df, y, x))

  expect_snapshot(error = TRUE, {
    df <- data.frame(x = 1, y = 2)

    f <- \(data, var1, var2) {
      check_with(data, {{ var1 }} < {{ var2 }})
    }
    f(df, y, x)
  })
})
