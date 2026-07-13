test_that("modifiers create classed objects", {
  expect_s3_class(
    bare(1), c("favr_bare", "favr_modifier"),
    exact = TRUE
  )

  expect_s3_class(
    at_least(1), c("favr_at_least", "favr_modifier"),
    exact = TRUE
  )
  expect_s3_class(
    at_most(1), c("favr_at_most", "favr_modifier"),
    exact = TRUE
  )
  expect_s3_class(
    in_range(1, 2), c("favr_in_range", "favr_modifier"),
    exact = TRUE
  )
})

test_that("length modifiers cast safely to integer", {
  expect_no_error(at_least(1.0))
  expect_no_error(at_most(1.0))
  expect_no_error(in_range(1.0, 2.0))
})

test_that("length modifiers error for non-castable objects", {
  expect_error(at_least(1.5))
  expect_error(at_most(1.5))
  expect_error(in_range(1.5, 2.5))

  expect_snapshot(error = TRUE, {
    at_least(mean)
    at_most(1.5)
    in_range("a", "b")
  })
})

test_that("length modifiers error for non-scalars", {
  expect_error(at_least(c(1, 2)))
  expect_error(at_most(c(1, 2)))
  expect_error(in_range(2, c(3, 4)))
  expect_error(in_range(c(1, 2), 4))
  expect_error(in_range(c(1, 2), c(3, 4)))

  expect_snapshot(error = TRUE, {
    at_least(c(1, 2))
    at_most(c(1, 2))
    in_range(c(1, 2), c(3, 4))
  })
})

test_that("length modifiers error for negative values", {
  expect_error(at_least(-1))
  expect_error(at_most(-1))
  expect_error(in_range(-1, 2))
  expect_error(in_range(1, -2))
  expect_error(in_range(-1, -2))

  expect_snapshot(error = TRUE, {
    at_least(-1)
    at_most(-1)
    in_range(-1, 2)
  })
})

test_that("in_range errors if n_max bigger than n_min", {
  expect_error(in_range(2, 1))

  expect_snapshot(error = TRUE, {
    in_range(2, 1)
  })
})

test_that("bare modifier correctly errors for non-bare objects", {
  expect_null(check_integer(factor(1)))
  expect_error(check_integer(bare(factor(1))))

  expect_snapshot(error = TRUE, {
    check_integer(bare(factor(1)))
  })
})

test_that("bare modifier does not intefere with allow_null", {
  expect_null(check_list(bare(NULL), allow_null = TRUE))
  expect_null(check_scalar_list(bare(NULL), allow_null = TRUE))
})

test_that("length modifiers correctly control length checks", {
  expect_null(check_atomic(1:5, n = at_least(3)))
  expect_error(check_atomic(1:5, n = at_least(10)))

  expect_null(check_atomic(1:5, n = at_most(10)))
  expect_error(check_atomic(1:5, n = at_most(3)))

  expect_null(check_atomic(1:5, n = in_range(2, 10)))
  expect_error(check_atomic(1:5, n = in_range(6, 10)))
  expect_error(check_atomic(1:5, n = in_range(2, 4)))

  expect_snapshot(error = TRUE, {
    check_atomic(1:5, n = at_least(10))
    check_atomic(1:5, n = at_most(3))
    check_atomic(1:5, n = in_range(6, 10))
    check_atomic(1:5, n = in_range(2, 4))
  })
})
