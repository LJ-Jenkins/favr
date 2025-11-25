test_that("recycle_if_not errors if unnamed arguments", {
  x <- 1:5
  y <- 1:5
  z <- 10
  expect_error(recycle_if_not(x = 10, y = 10, z))
  expect_error(recycle_if_not(x = 10, y = 10, 10))
})

test_that("recycle_if_not errors objects not in env", {
  a_env <- new.env()
  a_env$x <- 1
  a_env$y <- 1
  expect_error(recycle_if_not(x = 1))
  expect_no_error(recycle_if_not(x = 1, .env = a_env))
  expect_error(recycle_if_not(x = 1, z = 1, .env = a_env))
  expect_no_error(recycle_if_not(y = 5, .env = a_env))
  expect_identical(length(a_env$y), 5L)
})

test_that("recycle_if_not errors if size arguments aren't scalar integerish", {
  x <- 1:5
  y <- 1:5
  z <- 10
  expect_error(recycle_if_not(x = 10.1, y = 10))
  expect_error(recycle_if_not(x = 10, y = 10.1))
  expect_error(recycle_if_not(x = y))
})

test_that("recycle_if_not recycles correctly", {
  x <- 1
  y <- 5
  z <- 1
  recycle_if_not(x = 10, y = 5, z = vctrs::vec_size(x))
  expect_identical(length(x), 10L)
  expect_identical(length(y), 5L)
  expect_identical(length(z), 10L)
})

test_that("recycle_if_not works with injection", {
  x <- 1L
  z <- 5
  y <- "hello"
  x_name <- "x"
  y_list <- list(y = 10)
  expect_no_error(recycle_if_not(!!x_name := !!z))
  expect_no_error(recycle_if_not(!!!y_list))
  expect_no_error(recycle_if_not({{ x_name }} := !!z))
  expect_no_error(recycle_if_not({{ x_name }} := {{ z }}))
  expect_error(recycle_if_not({{ x_name }} := {{ y }}))
})

test_that("recycle_if_not selects correct environment", {
  x <- 1L
  e <- new.env()
  e$x <- 1L
  e$y <- 1L
  q <- "hi"
  z <- 15L
  expect_no_error(recycle_if_not(x = 5, .env = e))
  expect_identical(length(e$x), 5L)
  expect_identical(length(x), 1L)
  expect_no_error(recycle_if_not(x = 3))
  expect_identical(length(x), 3L)
  expect_error(recycle_if_not(y = q, .env = e))
  expect_no_error(recycle_if_not(y = z, .env = e))
  expect_identical(length(e$y), 15L)
})

test_that("recycle_if_not reverts variables correctly", {
  x <- y <- z <- 1L
  expect_error(recycle_if_not(x = 10, y = 5, z = -5))
  expect_identical(length(x), 1L)
  expect_identical(length(y), 1L)
})

test_that("recycle_if_not errors if recycle() or coerce() used", {
  x <- y <- 1L
  expect_error(recycle_if_not(x = recycle(1)))
  expect_error(recycle_if_not(y = coerce(size = 1)))
})
