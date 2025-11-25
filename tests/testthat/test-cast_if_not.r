test_that("cast_if_not errors if unnamed arguments", {
  x <- 1:5
  y <- 1:5
  z <- 1:5
  df <- data.frame(x, y, z)
  expect_error(cast_if_not(x = double(), y = double(), z))
  expect_error(cast_if_not(x = double(), y = double(), double()))
})

test_that("cast_if_not errors objects not in env", {
  a_env <- new.env()
  a_env$x <- 1
  a_env$y <- 5L
  expect_error(cast_if_not(x = double()))
  expect_no_error(cast_if_not(x = double(), .env = a_env))
  expect_error(cast_if_not(x = double(), z = double(), .env = a_env))
  expect_no_error(cast_if_not(y = double(), .env = a_env))
  expect_identical(class(a_env$y), "numeric")
})

test_that("cast_if_not casts correctly", {
  x <- 1L
  y <- 5L
  z <- 1L
  cast_if_not(x = double(), y = double(), z = y)
  expect_identical(class(x), "numeric")
  expect_identical(class(y), "numeric")
  expect_identical(class(z), "numeric")
  x <- 1L
  cast_if_not(x = lossy(integer()))
  expect_identical(class(x), "integer")
})

test_that("cast_if_not works with injection", {
  x <- 1L
  z <- numeric()
  y <- "hello"
  l <- quote(lossy(integer()))
  v <- 1.5
  v_name <- "v"
  x_name <- "x"
  x_list <- list(x = 1.5)
  expect_no_error(cast_if_not(!!x_name := !!z))
  expect_no_error(cast_if_not(!!!x_list))
  expect_no_error(cast_if_not({{ x_name }} := !!z))
  expect_no_error(cast_if_not({{ x_name }} := {{ z }}))
  expect_error(cast_if_not({{ x_name }} := {{ y }}))
  expect_no_error(cast_if_not(!!v_name := !!l))
  expect_identical(v, 1L)
})

test_that("cast_if_not selects correct environment", {
  x <- 1L
  e <- new.env()
  e$x <- 1L
  e$y <- 1.5
  z <- 1L
  expect_no_error(cast_if_not(x = double(), .env = e))
  expect_identical(class(e$x), "numeric")
  expect_identical(class(x), "integer")
  expect_no_error(cast_if_not(x = double()))
  expect_identical(class(x), "numeric")
  expect_error(cast_if_not(y = z, .env = e))
  expect_no_error(cast_if_not(y = lossy(z), .env = e))
  expect_identical(class(e$y), "integer")
})

test_that("cast_if_not lossy() works correctly", {
  x <- 1.5
  expect_no_error(cast_if_not(x = lossy(integer())))
  expect_error(cast_if_not(y = integer()))
})

test_that("cast_if_not reverts variables correctly", {
  x <- y <- z <- 1L
  expect_error(cast_if_not(x = double(), y = double(), z = character()))
  expect_identical(class(x), "integer")
  expect_identical(class(y), "integer")
})

test_that("cast_if_not errors if cast() or coerce() used", {
  x <- y <- z <- 1L
  expect_error(cast_if_not(x = cast(double())))
  expect_error(cast_if_not(y = coerce(double())))
})
