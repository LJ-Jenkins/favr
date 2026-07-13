test_that("walk_check() returns empty input", {
  expect_identical(walk_check(list(), is.numeric), list())
  expect_identical(walk_check(list(), ~ is.numeric(.x)), list())
})

test_that("walk_check() errors on non-vector input", {
  e <- new.env()
  e$x <- 1:3
  e$y <- 4:6
  expect_error(walk_check(e, is.numeric))
  expect_error(walk_check(mean, is.numeric))

  expect_snapshot(error = TRUE, {
    walk_check(e, is.numeric)
    walk_check(mean, is.numeric)
  })
})

test_that("walk_check() allows function, formula or string", {
  expect_no_error(walk_check(list(1, 2, 3), is.numeric))
  expect_no_error(walk_check(list(1, 2, 3), ~ is.numeric(.x)))
  expect_no_error(walk_check(list(1, 2, 3), "is.numeric"))
  expect_error(walk_check(list(1, 2, 3), 1))

  expect_snapshot(error = TRUE, {
    walk_check(list(1, 2, 3), 1)
  })
})

test_that("walk_check() returns input if all checks pass", {
  x <- list(1, 2, 3)
  expect_identical(walk_check(x, is.numeric), x)
  expect_identical(walk_check(x, ~ is.numeric(.x)), x)
})

test_that("walk_check() errors on non-logical returns", {
  expect_error(walk_check(list(1, 2, 3), ~"not logical"))
  expect_snapshot(error = TRUE, {
    walk_check(list(1, 2, 3), ~"not logical")
  })
})

test_that("walk_check() errors on NA returns", {
  expect_error(walk_check(list(1, 2, 3), ~NA))
  expect_error(walk_check(list(1, 2, 3), ~ c(TRUE, NA)))
  expect_snapshot(error = TRUE, {
    walk_check(list(1, 2, 3), ~NA)
    walk_check(list(1, 2, 3), ~ c(TRUE, NA))
  })
})

test_that("walk_check() errors when check fails", {
  expect_error(walk_check(list(1, 2, 3), ~ .x < 3))
  expect_snapshot(error = TRUE, {
    walk_check(list(1, 2, 3), ~ .x < 3)
  })
})

test_that("walk_check() errors show index and name (if present)", {
  expect_snapshot(error = TRUE, {
    walk_check(list(1, 2, my_named_element = 3), ~ .x < 3)
  })
})
