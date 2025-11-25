test_that("enforce schema usage", {
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

  x <- 20
  expect_error(enforce_schema(li2)) # can't recycle 10 to 20

  x <- 10
  expect_no_error(enforce_schema(li2))

  li2$x <- 1.5
  expect_error(enforce_schema(li2)) # y stays numeric, "will not trigger" fails

  df <- data.frame(x = 1, y = 2)

  df2 <- df |>
    schema(.names = c("x", "y"), .size = 1)

  names(df2)[2] <- "new_name"

  expect_error(enforce_schema(df2))

  df2 <- rbind(df2, df2)

  expect_error(enforce_schema(df2))
})

test_that("add to schema usage", {
  df <- data.frame(x = 1, y = 2)

  df2 <- schema(df, x == 1, y == 2, .names = "x")

  expect_error(add_to_schema(df2, x ~ recycle(3)))
  expect_error(add_to_schema(df2, .size = 3))

  expect_no_error(df3 <- add_to_schema(df2, .names = "y"))
  expect_identical(attr(df3, "schema")$mask_names, c("x", "y"))

  expect_no_error(df3 <- add_to_schema(df3, .size = 1))
  expect_identical(attr(df3, "schema")$mask_size, 1)

  expect_no_error(df3 <- add_to_schema(df3, .size = NULL))
  expect_identical(attr(df3, "schema")$mask_size, 1)

  expect_error(add_to_schema(df3, .names = "nope"))

  li <- list(x = 1, y = 2)
  msg1 <- "wow"
  msg2 <- "wowza"
  msg3 <- "woo"

  expect_no_error(
    li2 <- schema(li, "{.var {msg1}}" = x == 1, "{.fn {msg2}}" = y ~ ~ .x == 2)
  )
  expect_no_error(
    li3 <- add_to_schema(li2, "{msg3}" = c(x, y) ~ recycle(1))
  )
  expect_identical(
    attr(li3, "schema")$error_names, c("`wow`", "`wowza()`", "woo")
  )
})
