test_that("detects correct type", {
  expect_null(check_complex(complex(1)))
  expect_null(check_complex(bare(complex(1))))
  expect_null(check_complex(NA_complex_))
  expect_error(check_complex(1.1))
  expect_error(check_complex(TRUE))
  expect_error(check_complex(NULL))
  expect_error(check_complex(NA))
  x <- structure(complex(1), class = "my_complex")
  expect_error(check_complex(bare(x)))

  expect_null(check_scalar_complex(complex(1)))
  expect_null(check_scalar_complex(bare(complex(1))))
  expect_null(check_scalar_complex(NA_complex_))
  expect_error(check_scalar_complex(1.1))
  expect_error(check_scalar_complex(TRUE))
  expect_error(check_scalar_complex(NULL))
  expect_error(check_scalar_complex(NA))
  expect_error(check_scalar_complex(bare(x)))
})

test_that("n arg checks length correctly", {
  expect_null(check_complex(complex(2), n = 2))
  expect_error(check_complex(complex(2), n = 1))
  expect_error(check_complex(complex(2), n = 3))

  expect_null(check_complex(complex(2), n = at_least(1)))
  expect_error(check_complex(complex(2), n = at_least(3)))
  expect_null(check_complex(complex(2), n = at_most(3)))
  expect_error(check_complex(complex(2), n = at_most(1)))
  expect_null(check_complex(complex(2), n = in_range(1, 3)))
  expect_error(check_complex(complex(2), n = in_range(3, 5)))

  expect_null(check_scalar_complex(complex(1)))
  expect_error(check_scalar_complex(complex(2)))
})

test_that("allow_null works correctly", {
  expect_null(check_complex(NULL, allow_null = TRUE))
  expect_error(check_complex(NULL, allow_null = FALSE))

  expect_null(check_scalar_complex(NULL, allow_null = TRUE))
  expect_error(check_scalar_complex(NULL, allow_null = FALSE))

  expect_snapshot(error = TRUE, {
    check_complex(NULL, allow_null = FALSE)
    check_scalar_complex(NULL, allow_null = FALSE)
  })
})

test_that("finite works correctly", {
  expect_null(check_complex(as.complex(c(1, 2, Inf)), finite = FALSE))
  expect_error(check_complex(as.complex(c(1, 2, Inf)), finite = TRUE))
  expect_null(check_complex(as.complex(c(1, 2, NaN)), finite = FALSE))
  expect_error(check_complex(as.complex(c(1, 2, NaN)), finite = TRUE))

  expect_null(check_scalar_complex(as.complex(Inf), finite = FALSE))
  expect_error(check_scalar_complex(as.complex(Inf), finite = TRUE))
  expect_null(check_scalar_complex(as.complex(NaN), finite = FALSE))
  expect_error(check_scalar_complex(as.complex(NaN), finite = TRUE))

  expect_snapshot(error = TRUE, {
    check_complex(as.complex(c(1, 2, Inf)), finite = TRUE)
    check_complex(as.complex(c(1, 2, NaN)), finite = TRUE)
    check_scalar_complex(as.complex(Inf), finite = TRUE)
    check_scalar_complex(as.complex(NaN), finite = TRUE)
  })
})

test_that("error shows type problem preferentially", {
  expect_snapshot(error = TRUE, {
    check_complex(1.1, n = 2)
    check_complex(bare(structure(complex(1), class = "my_complex")), n = 2)
    check_complex(c(1.1, 2L), n = 1)
    check_scalar_complex(c(1.1, 2L))
    check_scalar_complex(bare(structure(complex(2), class = "my_complex")))
  })
})

test_that("error shows length problem when types match", {
  expect_snapshot(error = TRUE, {
    check_complex(complex(2), n = 1)
    check_scalar_complex(complex(2))
  })
})

test_that("arg is shown in error", {
  expect_snapshot(error = TRUE, {
    x <- 1.1
    check_complex(x)
    check_scalar_complex(x)
    check_complex(x, n = 2, arg = "my_arg")
    check_scalar_complex(x, arg = "my_arg")
  })
})

test_that("call is shown in error", {
  expect_snapshot(error = TRUE, {
    f <- function() {
      check_complex(1.1)
    }
    fs <- function() {
      check_scalar_complex(1.1)
    }
    f()
    fs()
  })
})

test_that("dots evaluated and passed to cli_abort/abort", {
  expect_snapshot(error = TRUE, {
    check_complex(1.1, footer = "Custom footer")
    check_scalar_complex(1.1, footer = "Custom footer")
  })
})

test_that(".envir is silently overriden", {
  expect_snapshot(error = TRUE, {
    e <- environment()
    e$arg <- "my_arg"
    check_complex(1.1, .envir = e)
    check_scalar_complex(1.1, .envir = e)
  })
})
