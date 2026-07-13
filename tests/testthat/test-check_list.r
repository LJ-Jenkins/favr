test_that("detects correct type", {
  expect_null(check_list(list()))
  expect_null(check_list(bare(list())))
  expect_null(check_scalar_list(list(1)))
  expect_null(check_scalar_list(bare(list(1))))
  expect_null(check_list(data.frame()))
  expect_null(check_scalar_list(data.frame(x = 1)))
  expect_null(
    check_list(lm(x ~ y, data = data.frame(x = 1, y = 1)))
  )
  expect_error(check_list(bare(data.frame())))
  expect_error(check_scalar_list(bare(data.frame(x = 1))))

  expect_error(check_list(expression(1)))
  expect_error(check_list(NULL))
  expect_error(check_list(mean))

  expect_error(check_scalar_list(expression(1)))
  expect_error(check_scalar_list(NULL))
  expect_error(check_scalar_list(mean))
})

test_that("n arg checks length correctly", {
  expect_null(check_list(list(1, 2), n = 2))
  expect_null(check_list(data.frame(x = 1:100), n = 1))
  expect_error(check_list(list(1, 2), n = 1))
  expect_error(check_list(list(1, 2), n = 3))
  expect_null(check_list(list(list(1, 2)), n = 1))

  expect_null(check_list(list(list(1, 2)), n = at_least(1)))
  expect_error(check_list(list(list(1, 2)), n = at_least(2)))
  expect_null(check_list(list(list(1, 2)), n = at_most(1)))
  expect_error(check_list(list(list(1, 2)), n = at_most(0)))
  expect_null(check_list(list(1, 2), n = in_range(1, 3)))
  expect_error(check_list(list(list(1, 2)), n = in_range(2, 3)))
  expect_null(check_list(data.frame(x = 1:10, y = 1:10), n = at_least(2)))

  expect_null(check_scalar_list(list(1)))
  expect_null(check_scalar_list(list(list(1, 2))))
  expect_null(check_scalar_list(data.frame(x = 1:100)))
  expect_error(check_scalar_list(list(1, 2)))
})

test_that("allow_null works correctly", {
  expect_null(check_list(NULL, allow_null = TRUE))
  expect_error(check_list(NULL, allow_null = FALSE))
  expect_null(check_list(list(NULL), allow_null = FALSE))

  expect_null(check_scalar_list(NULL, allow_null = TRUE))
  expect_error(check_scalar_list(NULL, allow_null = FALSE))
  expect_null(check_scalar_list(list(NULL), allow_null = FALSE))

  expect_snapshot(error = TRUE, {
    check_list(NULL, allow_null = FALSE)
    check_scalar_list(NULL, allow_null = FALSE)
  })
})

test_that("error shows type problem preferentially", {
  expect_snapshot(error = TRUE, {
    e <- new.env()
    e$x <- list(1)
    e$y <- 1
    check_list(e, n = 2)
    check_list(e, n = 1)
    check_scalar_list(e)
    check_list(bare(data.frame(x = 1)), n = 2)
    check_scalar_list(bare(data.frame(x = 1, y = 2)))
  })
})

test_that("error shows length problem when types match", {
  expect_snapshot(error = TRUE, {
    check_list(list(1, 2), n = 1)
    check_scalar_list(list(1, 2))
  })
})

test_that("arg is shown in error", {
  expect_snapshot(error = TRUE, {
    x <- quote(a + b)
    check_list(x)
    check_scalar_list(x)
    check_list(x, arg = "my_arg")
    check_scalar_list(x, arg = "my_arg")
  })
})

test_that("call is shown in error", {
  expect_snapshot(error = TRUE, {
    f <- function() {
      check_list(mean)
    }
    fs <- function() {
      check_scalar_list(mean)
    }
    f()
    fs()
  })
})

test_that("dots evaluated and passed to cli_abort/abort", {
  expect_snapshot(error = TRUE, {
    check_list(mean, footer = "Custom footer")
    check_scalar_list(mean, footer = "Custom footer")
  })
})

test_that(".envir is silently overriden", {
  expect_snapshot(error = TRUE, {
    e <- environment()
    e$arg <- "my_arg"
    check_list(mean, .envir = e)
    check_scalar_list(mean, .envir = e)
  })
})
