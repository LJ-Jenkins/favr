test_that("check_dir() and check_file() errors on non-string input", {
  expect_error(check_dir(c("w", "w")))
  expect_error(check_dir(1))
  expect_snapshot(error = TRUE, {
    check_dir(1)
    check_dir(c("w", "e"))
  })

  expect_error(check_file(1))
  expect_error(check_file(c("w", "e")))
  expect_snapshot(error = TRUE, {
    check_file(1)
    check_file(c("w", "e"))
  })
})

test_that("check_dir() and check_file() errors on non-existing paths", {
  expect_error(check_dir("non_existing_dir"))
  expect_error(check_file("non_existing_file"))
})

test_that("check_dir() and check_file() correctly identify existing paths", {
  d <- withr::local_tempdir()
  f <- withr::local_tempfile(lines = "x")
  expect_null(check_dir(d))
  expect_null(check_file(f))
})

test_that("check_dir() and check_file() don't duiplicate path if typed", {
  expect_snapshot(error = TRUE, {
    check_dir("non_existing_dir")
    check_file("non_existing_file")
    a_var <- "non_existing_dir"
    check_dir(a_var)
    check_file(a_var)
  })
})

test_that("check_dir() informative error if given filepath", {
  expect_snapshot(
    error = TRUE,
    {
      f <- withr::local_tempfile(lines = "x")
      check_dir(f)
    },
    transform = function(lines) gsub(f, "<file>", lines, fixed = TRUE)
  )
})

test_that("check_file() informative error if given dirpath", {
  expect_snapshot(
    error = TRUE,
    {
      d <- withr::local_tempdir()
      check_file(d)
    },
    # longer tmp paths can sometimes split lines so replace with a multi line
    transform = function(lines) gsub(d, strrep("<dir>", 50), lines, fixed = TRUE)
  )
})
