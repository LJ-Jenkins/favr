test_that("enforce `.error_call` and no args checks", {
  li <- list(x = 1)
  e <- 123
  expect_no_error(enforce())
  expect_error(enforce(li, .error_call = mean))
  expect_error(enforce(li, .error_call = e))
})

test_that("enforce basic usage", {
  x <- 1
  xx <- 1
  y <- 0
  yy <- 0

  expect_no_error(enforce(x == 1))
  expect_no_error(enforce(x ~ ~ .x == 1))
  expect_no_error(enforce(c(x) ~ \(.x) .x == 1))
  expect_no_error(enforce(x ~ function(.x) .x == 1))
  expect_no_error(enforce(c(x) ~ ~ .x == 1))
  expect_no_error(enforce(c(x, xx) ~ ~ .x == 1))
  expect_no_error(enforce(
    x ~ list(cast(integer()), recycle(10), ~ .x == 1, \(.x) .x == 1),
    xx ~ list(cast(integer()), recycle(length(x)), ~ .x == 1, \(.x) .x == 1)
  ))

  expect_no_error(enforce(y < x))
  expect_no_error(enforce(y ~ ~ .x < x))
  expect_no_error(enforce(c(y) ~ \(.x) .x < x))
  expect_no_error(enforce(y ~ function(.x) .x < x))
  expect_no_error(enforce(c(y) ~ ~ .x < x))
  expect_no_error(enforce(c(y, yy) ~ ~ .x < x))
  expect_no_error(enforce(
    y ~ list(coerce(type = integer(), size = 10), ~ .x == 0, \(.x) .x == 0),
    yy ~ list(cast(integer()), recycle(length(x)), ~ .x == 0, \(.x) .x == 0)
  ))

  expect_error(enforce(yy ~ cast(character())))
})

test_that("enforce errors with bad expressions/formulas", {
  x <- 1
  y <- 0
  xx <- 1
  yy <- 0

  expect_error(enforce(x + y))
  expect_error(enforce(~ x == 1))
  expect_error(enforce(x ~ y ~ x == 1))
  expect_error(enforce(x + y))
  expect_error(enforce(~ x == 1))
  expect_error(enforce(x ~ y ~ x == 1))
})

test_that("enforce casts correctly", {
  x <- 1L
  y <- 1.5

  expect_error(enforce(x ~ cast(character())))
  expect_error(enforce(y ~ cast(integer())))
  expect_error(enforce(x ~ coerce(character())))
  expect_error(enforce(y ~ coerce(integer())))

  enforce(x ~ cast(double()))
  expect_identical(class(x), "numeric")

  enforce(y ~ cast(integer(), lossy = TRUE))
  expect_identical(class(y), "integer")

  enforce(y ~ coerce(double()))
  expect_identical(class(y), "numeric")

  y <- 1.5
  enforce(y ~ coerce(integer(), lossy = TRUE))
  expect_identical(class(y), "integer")
})

test_that("enforce recycle correctly", {
  x <- 1L
  y <- 1.5
  z <- 1:3

  expect_error(enforce(x ~ recycle(-1)))
  expect_error(enforce(y ~ recycle(data.frame())))
  expect_error(enforce(x ~ recycle(1.5)))
  expect_error(enforce(z ~ recycle(6)))
  expect_error(enforce(y ~ coerce(size = data.frame())))
  expect_error(enforce(x ~ coerce(size = -2)))
  expect_error(enforce(x ~ coerce(size = 1.5)))
  expect_error(enforce(z ~ coerce(size = 6)))

  enforce(x ~ recycle(5))
  expect_identical(length(x), 5L)

  enforce(y ~ recycle(10))
  expect_identical(length(y), 10L)

  x <- 1
  enforce(x ~ coerce(size = 5))
  expect_identical(length(x), 5L)
})

test_that("enforce coerces correctly", {
  x <- 1L
  y <- 1.5
  z <- 3L

  expect_error(enforce(y ~ coerce(type = data.frame())))
  expect_error(enforce(x ~ coerce(size = 1.5)))
  expect_error(enforce(x ~ coerce(size = -1)))

  enforce(x ~ coerce(1.5, 5))
  expect_identical(class(x), "numeric")
  expect_identical(length(x), 5L)

  enforce(y ~ coerce(size = 10))
  expect_identical(class(y), "numeric")
  expect_identical(length(y), 10L)

  y <- 1.5
  enforce(y ~ coerce(type = integer(), size = 10, lossy = TRUE))
  expect_identical(class(y), "integer")
  expect_identical(length(y), 10L)

  x <- 1L
  y <- 1.5
  enforce(y ~ coerce(type = x, size = z, lossy = TRUE))
  expect_identical(class(y), "integer")
  expect_identical(length(y), 3L)
})

test_that("schema complex usage", {
  x <- 1L
  y <- 1.5
  z <- 3L
  xx <- 10.0

  enforce(
    y ~ coerce(type = x, size = xx, lossy = TRUE),
    z ~ recycle(5),
    x ~ recycle(xx),
    "will not trigger" = is.integer(y),
    "nope no trigger" = length(y) == 10,
    "also no trigger" = length(x) == 10
  )

  expect_identical(class(y), "integer")
  expect_identical(length(y), 10L)
  expect_identical(length(z), 5L)
  expect_identical(length(x), 10L)
})

test_that("enforce reverts variables correctly", {
  x <- y <- z <- 1L
  expect_error(enforce(c(x, y) ~ cast(double()), z ~ cast(character())))
  expect_identical(class(x), "integer")
  expect_identical(class(y), "integer")

  expect_error(enforce(x ~ recycle(10), y ~ recycle(5), z ~ recycle(-5)))
  expect_identical(length(x), 1L)
  expect_identical(length(y), 1L)
})

test_that("enforce errors if lossy() used", {
  x <- y <- z <- 1L
  expect_error(enforce(c(x, y) ~ cast(double()), z ~ lossy(character())))
})
