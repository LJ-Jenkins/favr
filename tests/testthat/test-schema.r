test_that("schema attaches `with_schema` class", {
  li <- list(x = 1, y = 0)
  df <- data.frame(x = 1, y = 0)
  li1 <- schema(li, x == 1)
  df1 <- schema(df, x == 1)
  expect_true("with_schema" %in% class(li1))
  expect_true("with_schema" %in% class(df1))
})

test_that("schema returns input with no args", {
  li <- list(x = 1, y = 0)
  expect_no_error(schema(li))
  expect_error(schema())
})

test_that("schema errors with wrong types", {
  expect_error(schema(new.env()))
  expect_error(schema(c("hi" = 1, "there" = 2)))
  expect_error(schema(c("wow", "such", "vector")))
  expect_error(schema(1:10))
  expect_error(schema(mean))
})

test_that("schema `.error_call` checks", {
  li <- list(x = 1)
  e <- 123
  expect_error(schema(li, .error_call = mean))
  expect_error(schema(li, .error_call = e))
})

test_that("schema `.darg` checks", {
  li <- list(x = 1)
  expect_error(schema(li, .darg = mean))
})

test_that("schema `.size` arg checks", {
  li <- list(x = 1, y = 1, z = 1)
  df <- data.frame(x = 1:2, y = 1:2)
  x <- 3
  y <- 2

  expect_no_error(schema(li, .size = 3))
  expect_no_error(schema(df, .size = 2))
  expect_no_error(schema(li, .size = x))
  expect_no_error(schema(df, .size = y))
  expect_error(schema(li, .size = 3.5))
  expect_error(schema(df, .size = 2.5))
  expect_error(schema(li, .size = y))
  expect_error(schema(df, .size = x))
})

test_that("schema `.names` arg checks", {
  li <- list(x = 1, y = 1, z = 1)
  df <- data.frame(x = 1, y = 1)
  x <- c("x", "y")
  y <- c("x", "y", "z", "a")

  expect_no_error(schema(li, .names = x))
  expect_no_error(schema(df, .names = x))
  expect_no_error(schema(li, .names = "z"))
  expect_no_error(schema(df, .names = "y"))
  expect_no_error(schema(df, .names = NULL))
  expect_no_error(schema(df, .names = character()))
  expect_error(schema(li, .names = y))
  expect_error(schema(df, .names = y))
})

test_that("schema basic usage", {
  li <- list(x = 1, y = 0)
  df <- data.frame(x = 1, y = 0)

  expect_no_error(schema(li, x == 1))
  expect_no_error(schema(li, x ~ ~ .x == 1))
  expect_no_error(schema(li, c(x) ~ \(.x) .x == 1))
  expect_no_error(schema(li, starts_with("x") ~ function(.x) .x == 1))
  expect_no_error(schema(li, contains("x") ~ ~ .x == 1))
  expect_no_error(schema(li, matches("x") ~ \(.x) .x == 1))
  expect_no_error(schema(df, x ~ function(.x) .x == 1))
  expect_no_error(schema(df, c(x) ~ ~ .x == 1))
  expect_no_error(schema(df, starts_with("x") ~ \(.x) .x == 1))
  expect_no_error(schema(df, contains("x") ~ function(.x) .x == 1))
  expect_no_error(schema(df, matches("x") ~ ~ .x == 1))

  expect_no_error(schema(li, y < x))
  expect_no_error(schema(li, y ~ ~ .x < x))
  expect_no_error(schema(li, c(y) ~ \(.x) .x < x))
  expect_no_error(schema(li, starts_with("y") ~ function(.x) .x < x))
  expect_no_error(schema(li, contains("y") ~ ~ .x < x))
  expect_no_error(schema(li, matches("y") ~ \(.x) .x < x))
  expect_no_error(schema(df, y ~ function(.x) .x < x))
  expect_no_error(schema(df, c(y) ~ ~ .x < x))
  expect_no_error(schema(df, starts_with("y") ~ \(.x) .x < x))
  expect_no_error(schema(df, contains("y") ~ function(.x) .x < x))
  expect_no_error(schema(df, matches("y") ~ ~ .x < x))

  expect_equal(
    schema(df, matches("y") ~ ~ .x < x),
    df,
    ignore_attr = TRUE
  )
})

test_that("schema errors with bad expressions/formulas", {
  li <- list(x = 1, y = 0)
  df <- data.frame(x = 1, y = 0)

  expect_error(schema(li, x + y))
  expect_error(schema(li, ~ x == 1))
  expect_error(schema(li, x ~ y ~ x == 1))
  expect_error(schema(df, x + y))
  expect_error(schema(df, ~ x == 1))
  expect_error(schema(df, x ~ y ~ x == 1))
})

test_that("schema casts correctly", {
  li <- list(x = 1L, y = 1.5)
  df <- data.frame(x = 1L, y = 1.5)

  expect_error(schema(li, x ~ cast(character())))
  expect_error(schema(df, x ~ cast(character())))
  expect_error(schema(li, y ~ cast(integer())))
  expect_error(schema(df, y ~ cast(integer())))
  expect_error(schema(li, x ~ coerce(character())))
  expect_error(schema(df, x ~ coerce(character())))
  expect_error(schema(li, y ~ coerce(integer())))
  expect_error(schema(df, y ~ coerce(integer())))

  expect_identical(
    class(schema(li, x ~ cast(double()))$x),
    "numeric"
  )
  expect_identical(
    class(schema(df, x ~ cast(double()))$x),
    "numeric"
  )
  expect_identical(
    class(schema(li, x ~ cast(integer(), lossy = TRUE))$x),
    "integer"
  )
  expect_identical(
    class(schema(df, y ~ cast(integer(), lossy = TRUE))$y),
    "integer"
  )
  expect_identical(
    class(schema(li, y ~ coerce(double()))$y),
    "numeric"
  )
  expect_identical(
    class(schema(df, x ~ coerce(double()))$x),
    "numeric"
  )
  expect_identical(
    class(schema(li, y ~ coerce(integer(), lossy = TRUE))$y),
    "integer"
  )
  expect_identical(
    class(schema(df, y ~ coerce(integer(), lossy = TRUE))$y),
    "integer"
  )
  expect_identical(
    class(schema(df, y ~ coerce(x, lossy = TRUE))$y),
    "integer"
  )
})

test_that("schema recycle correctly", {
  li <- list(x = 1L, y = 1.5, z = 3L)
  df <- data.frame(x = 1L, y = 1.5)

  expect_error(schema(df, x ~ recycle(1)))
  expect_error(schema(li, y ~ recycle(data.frame())))
  expect_error(schema(li, x ~ recycle(1.5)))
  expect_error(schema(li, x ~ recycle(-1)))
  expect_error(schema(df, x ~ coerce(size = 1)))
  expect_error(schema(li, y ~ coerce(size = data.frame())))
  expect_error(schema(li, x ~ coerce(size = -2)))
  expect_error(schema(li, x ~ coerce(size = 1.5)))

  expect_identical(
    length(schema(li, x ~ recycle(5))$x),
    5L
  )
  expect_identical(
    length(schema(li, y ~ recycle(10))$y),
    10L
  )
  expect_identical(
    length(schema(li, x ~ recycle(5))$x),
    5L
  )
  expect_identical(
    lengths(schema(li, x ~ recycle(5), y ~ recycle(10))),
    c(x = 5L, y = 10L, z = 1L)
  )
  expect_identical(
    length(schema(li, x ~ coerce(size = 5))$x),
    5L
  )
  expect_identical(
    length(schema(li, x ~ recycle(z))$x),
    3L
  )
})

test_that("schema coerces correctly", {
  li <- list(x = 1L, y = 1.5, z = 3L)
  df <- data.frame(x = 1L, y = 1.5)

  expect_error(schema(df, x ~ coerce(size = 1)))
  expect_error(schema(li, y ~ coerce(type = data.frame())))
  expect_error(schema(li, x ~ coerce(size = 1.5)))
  expect_error(schema(li, x ~ coerce(size = -1)))

  li2 <- schema(li, x ~ coerce(1.5, 5))
  expect_identical(class(li2$x), "numeric")
  expect_identical(length(li2$x), 5L)

  li2 <- schema(li, y ~ coerce(size = 10))
  expect_identical(class(li2$y), "numeric")
  expect_identical(length(li2$y), 10L)

  li2 <- schema(li, y ~ coerce(type = integer(), size = 10, lossy = TRUE))
  expect_identical(class(li2$y), "integer")
  expect_identical(length(li2$y), 10L)

  li2 <- schema(li, y ~ coerce(type = x, size = z, lossy = TRUE))
  expect_identical(class(li2$y), "integer")
  expect_identical(length(li2$y), 3L)
})

test_that("schema complex usage", {
  li <- list(x = 1L, y = 1.5, z = 3L)
  x <- 10.0

  li2 <- li |>
    schema(
      y ~ coerce(type = .data$x, size = .env$x, lossy = TRUE),
      z ~ recycle(5),
      x ~ recycle(.env$x),
      "will not trigger" = is.integer(y),
      "nope no trigger" = length(y) == 10,
      "also no trigger" = length(x) == 10
    )

  expect_identical(class(li2$y), "integer")
  expect_identical(length(li2$y), 10L)
  expect_identical(length(li2$z), 5L)
  expect_identical(length(li2$x), 10L)
})
