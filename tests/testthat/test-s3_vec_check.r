test_that("detects correct inheritance", {
  fctr <- factor(c("a", "b", "c"))
  ord <- ordered(c("a", "b", "c"))
  date <- as.Date("2023-01-01")
  expect_null(s3_vec_check(fctr, NULL, "factor"))
  expect_null(s3_vec_check(ord, NULL, "ordered"))
  expect_null(s3_vec_check(date, NULL, "Date"))

  x <- structure(1, class = c("my_factor", "factor"))
  expect_null(s3_vec_check(x, NULL, "factor"))
  x <- structure(1, class = c("my_ordered", "ordered"))
  expect_null(s3_vec_check(x, NULL, "ordered"))
  x <- structure(1, class = c("my_date", "Date"))
  expect_null(s3_vec_check(x, NULL, "Date"))

  expect_error(s3_vec_check(bare(x), NULL, "Date"))

  expect_error(s3_vec_check(fctr, NULL, "Date"))
  expect_error(s3_vec_check(date, NULL, "ordered"))
  expect_error(s3_vec_check(date, NULL, "factor"))
})

test_that("n arg checks length correctly", {
  x <- factor(c("a", "b"))
  expect_null(s3_vec_check(x, n = 2, "factor"))
  expect_error(s3_vec_check(x, n = 1, "factor"))
  expect_error(s3_vec_check(x, n = 3, "factor"))

  expect_null(s3_vec_check(x, n = at_least(1), "factor"))
  expect_error(s3_vec_check(x, n = at_least(3), "factor"))
  expect_null(s3_vec_check(x, n = at_most(3), "factor"))
  expect_error(s3_vec_check(x, n = at_most(1), "factor"))
  expect_null(s3_vec_check(x, n = in_range(1, 3), "factor"))
  expect_error(s3_vec_check(x, n = in_range(3, 5), "factor"))

  expect_snapshot(error = TRUE, {
    s3_vec_check(x, n = 1, "factor")
    s3_vec_check(x, n = at_least(3), "factor", "a {.cls factor} vector")
    s3_vec_check(x, n = at_most(1), "factor")
    s3_vec_check(x, n = in_range(3, 5), "factor", "a {.cls factor} vector")
  })
})

test_that("nrow and ncol args check length correctly for table", {
  x <- table(state.division, state.region)
  expect_null(check_table(x, nrow = 9))
  expect_null(check_table(x, ncol = 4))
  expect_null(check_table(x, nrow = at_least(5)))
  expect_null(check_table(x, ncol = at_most(5)))
  expect_null(check_table(x, nrow = in_range(5, 10)))

  expect_error(check_table(x, nrow = 10))
  expect_error(check_table(x, ncol = 5))
  expect_error(check_table(x, nrow = at_least(10)))
  expect_error(check_table(x, ncol = at_most(3)))
  expect_error(check_table(x, nrow = in_range(10, 15)))
})

test_that("allow_null works correctly", {
  expect_null(s3_vec_check(NULL, n = NULL, "factor", allow_null = TRUE))
  expect_null(s3_vec_check(bare(NULL), n = NULL, "factor", allow_null = TRUE))
  expect_error(s3_vec_check(NULL, n = NULL, "factor", allow_null = FALSE))

  expect_snapshot(error = TRUE, {
    s3_vec_check(NULL, n = NULL, "factor", allow_null = FALSE)
    s3_vec_check(
      NULL,
      n = NULL,
      "factor",
      "{.cls factor} vector",
      allow_null = FALSE
    )
  })
})

test_that("error shows type problem preferentially", {
  expect_snapshot(error = TRUE, {
    s3_vec_check(list(1), n = 2, "Date")
    s3_vec_check(
      bare(structure(1.1, class = c("c1", "c2"))),
      n = 2, "c1"
    )
    s3_vec_check(c("a", "b"), n = 1, "Date")
  })
})

test_that("error shows length problem when types match", {
  expect_snapshot(error = TRUE, {
    s3_vec_check(c(1.1, 2.2), n = 1, "Date")
  })
})

test_that("arg is shown in error", {
  expect_snapshot(error = TRUE, {
    x <- 1L
    s3_vec_check(x, NULL, "factor")
    s3_vec_check(x, n = 2, "factor", arg = "my_arg")
  })
})

test_that("call is shown in error", {
  expect_snapshot(error = TRUE, {
    f <- function() {
      s3_vec_check("a", NULL, "Date")
    }
    f()
  })
})

test_that("dots passed to cli_abort/abort", {
  expect_snapshot(error = TRUE, {
    s3_vec_check("a", NULL, "Date", footer = "Custom footer")
  })
})

test_that(".envir doesn't interfere", {
  expect_snapshot(error = TRUE, {
    e <- environment()
    e$arg <- "my_arg"
    s3_vec_check("a", NULL, "Date", .envir = e)
  })
})
