test_that(
  "check_ext(), check_dir() and check_file() errors on non-string input",
  {
    expect_error(check_ext(1, ext = ".csv"))
    expect_error(check_ext(c("w", "e"), ext = ".csv"))
    expect_snapshot(error = TRUE, {
      check_ext(1, ext = ".csv")
      check_ext(c("w", "e"), ext = ".csv")
    })

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
  }
)

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

test_that("check_dir() and check_file() don't duplicate path if typed", {
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

test_that("check_ext() and check_file() errors on wrong extension", {
  f <- withr::local_tempfile(lines = "x", fileext = ".tmp")
  expect_error(check_ext(f, ext = ".csv"))
  expect_error(check_file(f, ext = ".csv"))
  expect_snapshot(error = TRUE, {
    check_ext(f, ext = ".csv")
    check_file(f, ext = ".csv")
  })

  expect_error(check_ext(f, ext = c(".csv", ".xlsx")))
  expect_error(check_file(f, ext = c(".csv", ".xlsx")))
  expect_snapshot(error = TRUE, {
    check_ext(f, ext = c(".csv", ".xlsx"))
    check_file(f, ext = c(".csv", ".xlsx"))
  })
})

test_that(
  "check_ext() and check_file() errors on wrong extension with case sensitivity",
  {
    f <- withr::local_tempfile(lines = "x", fileext = ".tmp")
    expect_error(check_ext(f, ext = ".CSV"))
    expect_error(check_file(f, ext = ".CSV"))
    expect_snapshot(error = TRUE, {
      check_ext(f, ext = ".CSV")
      check_file(f, ext = ".CSV")
    })

    expect_error(check_ext(f, ext = c(".CSV", ".XLSX")))
    expect_error(check_file(f, ext = c(".CSV", ".XLSX")))
    expect_snapshot(error = TRUE, {
      check_ext(f, ext = c(".CSV", ".XLSX"))
      check_file(f, ext = c(".CSV", ".XLSX"))
    })
  }
)

test_that("check_ext() and check_file() do not error on correct extension", {
  f <- withr::local_tempfile(lines = "x", fileext = ".tmp")
  expect_null(check_ext(f, ext = ".tmp"))
  expect_null(check_ext(f, ext = c(".tmp", ".csv")))
  expect_null(check_ext(f, ext = c(".csv", ".tmp")))
  expect_null(check_file(f, ext = ".tmp"))
  expect_null(check_file(f, ext = c(".tmp", ".csv")))
  expect_null(check_file(f, ext = c(".csv", ".tmp")))
})

test_that(
  "check_ext() and check_file() do not error on correct extension with case insensitivity",
  {
    f <- withr::local_tempfile(lines = "x", fileext = ".tmp")
    expect_null(check_ext(f, ext = ".TMP", case = FALSE))
    expect_null(check_ext(f, ext = c(".TMP", ".CSV"), case = FALSE))
    expect_null(check_ext(f, ext = c(".CSV", ".TMP"), case = FALSE))
    expect_null(check_file(f, ext = ".TMP", case = FALSE))
    expect_null(check_file(f, ext = c(".TMP", ".CSV"), case = FALSE))
    expect_null(check_file(f, ext = c(".CSV", ".TMP"), case = FALSE))
  }
)

test_that("check_ext() errors on non-chr or empty string ext input", {
  expect_error(check_ext("a", ext = 1))
  expect_error(check_ext("a", ext = c(".csv", "")))
  expect_error(check_ext("a", ext = c(".csv", NA)))
  expect_error(check_ext("a", ext = character(0)))
  expect_snapshot(error = TRUE, {
    check_ext("a", ext = 1)
    check_ext("a", ext = c(".csv", ""))
    check_ext("a", ext = c(".csv", NA))
    check_ext("a", ext = character(0))
  })
})

test_that("check_ext() works simply on strings", {
  expect_null(check_ext("a_b_end", ext = "end"))
})
