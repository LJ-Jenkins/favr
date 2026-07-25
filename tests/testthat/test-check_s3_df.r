test_that("detects correct inheritance", {
  df <- data.frame(x = 1)
  tbl <- structure(df, class = c("tbl_df", "tbl", "data.frame"))
  dttbl <- structure(df, class = c("data.table", "data.frame"))

  expect_null(check_s3_df(df, nrow = NULL, ncol = NULL, type = "data.frame"))
  expect_null(check_s3_df(tbl, nrow = NULL, ncol = NULL, type = "tbl_df"))
  expect_null(check_s3_df(dttbl, nrow = NULL, ncol = NULL, type = "data.table"))

  x <- structure(df, class = c("my_df", "data.frame"))
  expect_null(check_s3_df(x, nrow = NULL, ncol = NULL, type = "data.frame"))
  x <- structure(df, class = c("my_tbl", "tbl_df"))
  expect_null(check_s3_df(x, nrow = NULL, ncol = NULL, type = "tbl_df"))
  x <- structure(df, class = c("my_dt", "data.table"))
  expect_null(check_s3_df(x, nrow = NULL, ncol = NULL, type = "data.table"))

  expect_error(check_s3_df(bare(x), nrow = NULL, ncol = NULL, type = "data.table"))

  expect_error(check_s3_df(1, nrow = NULL, ncol = NULL, type = "data.frame"))
  expect_error(check_s3_df(1, nrow = NULL, ncol = NULL, type = "tbl_df"))
  expect_error(check_s3_df(1, nrow = NULL, ncol = NULL, type = "data.table"))
})

test_that("nrow and ncol checks length correctly", {
  x <- data.frame(x = 1:3, y = 1:3)
  expect_null(check_s3_df(x, nrow = 3, ncol = NULL, type = "data.frame"))
  expect_error(check_s3_df(x, nrow = 2, ncol = NULL, type = "data.frame"))

  expect_null(check_s3_df(x, nrow = NULL, ncol = 2, type = "data.frame"))
  expect_error(check_s3_df(x, nrow = NULL, ncol = 4, type = "data.frame"))

  expect_null(check_s3_df(x, nrow = at_least(2), ncol = NULL, type = "data.frame"))
  expect_error(check_s3_df(x, nrow = at_least(4), ncol = NULL, type = "data.frame"))

  expect_null(check_s3_df(x, nrow = NULL, ncol = at_least(1), type = "data.frame"))
  expect_error(check_s3_df(x, nrow = NULL, ncol = at_least(3), type = "data.frame"))

  expect_null(check_s3_df(x, nrow = at_most(3), ncol = NULL, type = "data.frame"))
  expect_error(check_s3_df(x, nrow = at_most(2), ncol = NULL, type = "data.frame"))

  expect_null(check_s3_df(x, nrow = NULL, ncol = at_most(3), type = "data.frame"))
  expect_error(check_s3_df(x, nrow = NULL, ncol = at_most(1), type = "data.frame"))

  expect_null(check_s3_df(x, nrow = in_range(2, 4), ncol = NULL, type = "data.frame"))
  expect_error(check_s3_df(x, nrow = in_range(4, 5), ncol = NULL, type = "data.frame"))

  expect_null(check_s3_df(x, nrow = NULL, ncol = in_range(1, 3), type = "data.frame"))
  expect_error(check_s3_df(x, nrow = NULL, ncol = in_range(3, 5), type = "data.frame"))
})

test_that("allow_null works correctly", {
  expect_null(check_s3_df(NULL, nrow = NULL, ncol = NULL, allow_null = TRUE, type = "data.frame"))
  expect_null(check_s3_df(bare(NULL), nrow = NULL, ncol = NULL, allow_null = TRUE, type = "data.frame"))
  expect_error(check_s3_df(NULL, nrow = NULL, ncol = NULL, allow_null = FALSE, type = "data.frame"))

  expect_snapshot(error = TRUE, {
    check_s3_df(NULL, nrow = NULL, ncol = NULL, allow_null = FALSE, type = "data.frame")
    check_s3_df(
      NULL,
      nrow = NULL,
      ncol = NULL,
      allow_null = FALSE,
      type = "data.frame",
      "{.cls data.frame} obj"
    )
  })
})

test_that("error shows type problem preferentially", {
  expect_snapshot(error = TRUE, {
    check_s3_df(list(1), nrow = 2, ncol = NULL, type = "data.frame")
    check_s3_df(
      bare(structure(1.1, class = c("c1", "c2"))),
      nrow = 2, ncol = NULL, type = "c1"
    )
    check_s3_df(c("a", "b"), nrow = 1, ncol = NULL, type = "data.frame")
  })
})

test_that("error shows length problem when types match", {
  expect_snapshot(error = TRUE, {
    check_s3_df(data.frame(x = 1:2), nrow = 1, ncol = NULL, type = "data.frame")
    check_s3_df(data.frame(x = 1:2), nrow = NULL, ncol = 2, type = "data.frame")
  })
})

test_that("arg is shown in error", {
  expect_snapshot(error = TRUE, {
    x <- 1L
    check_s3_df(x, nrow = NULL, ncol = NULL, type = "data.frame")
    check_s3_df(x, nrow = 2, ncol = NULL, type = "data.frame", arg = "my_arg")
  })
})

test_that("call is shown in error", {
  expect_snapshot(error = TRUE, {
    f <- function() {
      check_s3_df("a", nrow = NULL, ncol = NULL, type = "data.frame")
    }
    f()
  })
})

test_that("dots passed to cli_abort/abort", {
  expect_snapshot(error = TRUE, {
    check_s3_df("a", nrow = NULL, ncol = NULL, type = "data.frame", footer = "Custom footer")
  })
})

test_that(".envir doesn't interfere", {
  expect_snapshot(error = TRUE, {
    e <- environment()
    e$arg <- "my_arg"
    check_s3_df("a", nrow = NULL, ncol = NULL, type = "data.frame", .envir = e)
  })
})
