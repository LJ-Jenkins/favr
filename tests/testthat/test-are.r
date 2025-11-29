expect_tf <- function(result, expected) {
  expect_equal(unname(result), expected)
}

test_that("are_* errors if optional args aren't an atomic vector/list", {
  expect_error(
    are_list(list(), .n = mean)
  )
  expect_error(
    are_list(list(), list(), list(), .n = new.env())
  )
  expect_error(
    are_double(1, .finite = mean)
  )
  expect_error(
    are_double(1, 1, 1, .finite = new.env())
  )
})

test_that("are_* errors if optional args cannot be recycled", {
  expect_error(
    are_list(list(), .n = c(1, 2))
  )
  expect_error(
    are_list(list(), list(), list(), .n = c(1, 2))
  )
  expect_error(
    are_double(1, .finite = c(NA, Inf))
  )
  expect_error(
    are_double(1, 1, 1, .finite = c(-Inf, NaN))
  )
})

test_that(
  "are_* errors if optional args' names aren't present/more names than inputs",
  {
    x <- list(1)
    expect_error(
      are_list(x, .n = c(y = 1))
    )
    expect_error(
      are_list(x, .n = c(x = 1, y = 1))
    )
    expect_error(
      are_double(x, .finite = c(y = 1))
    )
    expect_error(
      are_double(x, .finite = c(x = 1, y = 1))
    )
  }
)

test_that("are_* errors if optional args aren't uniquely named", {
  x <- list(1)
  expect_error(
    are_list(x, .n = c(x = 1, x = 1))
  )
  expect_error(
    are_double(x, .finite = c(x = 1, x = 1))
  )
})

test_that(
  "are_* errors if dot args aren't uniquely named & named optional arg used",
  {
    x <- list(1)
    expect_error(
      are_list(x, x, .n = c(x = 1))
    )
    expect_error(
      are_double(x, x, .finite = c(x = 1))
    )
  }
)

test_that("are_* arguments recycle and apply names correctly", {
  expect_tf(
    are_list(list(), list(1), list(1, 2), list(1, 2, 3), .n = 3),
    c(FALSE, FALSE, FALSE, TRUE)
  )
  expect_tf(
    are_list(list(), list(1), list(1, 2), list(1, 2, 3), .n = 1),
    c(FALSE, TRUE, FALSE, FALSE)
  )

  a <- list()
  b <- list(1)
  c <- list(1, 2)
  d <- list(1, 2, 3)

  expect_tf(
    are_list(a, b, c, d, .n = c(d = 2)),
    c(TRUE, TRUE, TRUE, FALSE)
  )
  expect_tf(
    are_list(a, b, c, d, .n = list(b = 3, d = 2)),
    c(TRUE, FALSE, TRUE, FALSE)
  )

  expect_error(are_string("foo", .string = c("foo", "bar")))
  expect_no_error(are_string("foo", .string = list(c("foo", "bar"))))
})

test_that("are_* .all argument", {
  expect_equal(
    are_list(list(), list(1), list(1, 2), list(1, 2, 3), .all = TRUE),
    TRUE
  )
  expect_tf(
    are_list(list(), list(1), 1, list(1, 2, 3), .all = FALSE),
    c(TRUE, TRUE, FALSE, TRUE)
  )
  expect_error(
    are_list(list(), list(1), list(1, 2), list(1, 2, 3), .all = 1)
  )
  expect_error(
    are_list(list(), list(1), list(1, 2), list(1, 2, 3), .all = mean)
  )
})

test_that("are_* named", {
  x <- c(a = 1, b = 2)
  expect_tf(are_named(x, c(a = 1, 2)), c(TRUE, FALSE))
  expect_equal(are_named(x, c(a = 1, 2), .all = TRUE), FALSE)
  expect_tf(are_named(list(), vector()), c(FALSE, FALSE))
  expect_tf(are_named2(list(), vector()), c(TRUE, TRUE))

  y <- c(a = 1, 2)
  expect_equal(class(have_names(y)), "list")
  expect_tf(
    have_names(x, y, c(a = 1, 2, 3)),
    list(c(TRUE, TRUE), c(TRUE, FALSE), c(TRUE, FALSE, FALSE))
  )
  expect_equal(have_names(x, y, c(a = 1, 2, 3), .all = TRUE), FALSE)


  invalid <- setNames(letters[1:5], letters[1:5])
  names(invalid)[1] <- ""
  names(invalid)[3] <- NA

  expect_tf(are_named(invalid), FALSE)
  expect_tf(have_names(invalid), list(c(FALSE, TRUE, FALSE, TRUE, TRUE)))


  mat <- matrix(1:4, 2)
  colnames(mat) <- c("a", "b")
  expect_tf(are_named(mtcars, mat), c(TRUE, FALSE))
  expect_tf(
    have_names(mtcars, mat),
    list(rep(TRUE, 11), rep(FALSE, 4))
  )
})

#-- brought across from rlang/tests/testthat/test-types.R

test_that("predicates match definitions", {
  expect_true(are_character(letters, .n = 26))
  expect_false(are_character(letters, .n = 1))
  expect_false(are_list(letters, .n = 26))

  expect_true(are_list(mtcars, .n = 11))
  expect_false(are_list(mtcars, .n = 0))
  expect_false(are_double(mtcars, .n = 11))

  expect_true(are_complex(cpl(1, 2), .n = 2))
  expect_false(are_complex(cpl(1, 2), .n = 3))
  expect_false(are_scalar_complex(cpl(1, 2)))
  expect_false(are_bare_complex(structure(cpl(1, 2), class = "foo")))
})

test_that("are_integerish() heeds type requirement", {
  for (n in 0:2) {
    expect_true(are_integerish(integer(n)))
    expect_true(are_integerish(double(n)))
    expect_false(are_integerish(double(n + 1) + .000001))
  }

  types <- c("logical", "complex", "character", "expression", "list", "raw")
  for (type in types) {
    expect_false(are_integerish(vector(type)))
  }
})

test_that("are_integerish() heeds length requirement", {
  for (n in 0:2) {
    expect_true(are_integerish(double(n), .n = n))
    expect_false(are_integerish(double(n), .n = n + 1))
  }
})

test_that("non .finite double values are integerish", {
  expect_true(are_integerish(dbl(1, Inf, -Inf, NaN), .finite = NULL))
  expect_true(are_integerish(dbl(1, NA)))
  expect_true(are_integerish(int(1, NA)))
})

test_that("check .finiteness", {
  expect_true(are_double(dbl(1, 2), .finite = TRUE))
  expect_true(are_complex(cpl(1, 2), .finite = TRUE))
  expect_true(are_integerish(dbl(1, 2), .finite = TRUE))

  expect_false(are_double(dbl(1, 2), .finite = FALSE))
  expect_false(are_complex(cpl(1, 2), .finite = FALSE))
  expect_false(are_integerish(dbl(1, 2), .finite = FALSE))

  expect_false(are_double(dbl(1, Inf), .finite = TRUE))
  expect_false(are_complex(cpl(1, Inf), .finite = TRUE))
  expect_false(are_integerish(dbl(1, Inf), .finite = TRUE))

  expect_true(are_double(dbl(1, Inf), .finite = FALSE))
  expect_true(are_complex(cpl(1, Inf), .finite = FALSE))
  expect_true(are_integerish(dbl(1, Inf), .finite = FALSE))

  expect_true(are_double(dbl(-Inf, Inf), .finite = FALSE))
  expect_true(are_complex(cpl(-Inf, Inf), .finite = FALSE))
  expect_true(are_integerish(dbl(-Inf, Inf), .finite = FALSE))
})

test_that("scalar predicates heed type and length", {
  expect_true_false <- function(result) {
    expect_equal(unname(result), c(TRUE, FALSE, FALSE))
  }

  expect_true_false(are_scalar_list(list(1), list(1, 2), logical(1)))
  expect_true_false(are_scalar_atomic(logical(1), logical(2), list(1)))
  expect_true_false(are_scalar_vector(list(1), list(1, 2), quote(x)))
  expect_true_false(are_scalar_vector(logical(1), logical(2), function() {
  }))
  expect_true_false(are_scalar_integer(integer(1), integer(2), double(1)))
  expect_true_false(are_scalar_double(double(1), double(2), integer(1)))
  expect_true_false(
    are_scalar_character(character(1), character(2), logical(1))
  )
  expect_true_false(are_string(character(1), character(2), logical(1)))
  expect_true_false(are_scalar_logical(logical(1), logical(2), character(1)))
  expect_true_false(are_scalar_raw(raw(1), raw(2), NULL))
  expect_true_false(are_scalar_bytes(raw(1), raw(2), NULL))
})

test_that("are_integerish() supports large numbers (#578)", {
  expect_true(are_integerish(1e10))
  expect_true(are_integerish(2^52))
  expect_true(are_integerish(-2^52))

  expect_false(are_integerish(2^52 + 1))
  expect_false(are_integerish(-2^52 - 1))

  expect_false(are_integerish(2^50 - 0.1))
  expect_false(are_integerish(2^49 - 0.05))
  expect_false(are_integerish(2^40 - 0.0001))

  expect_false(are_integerish(-2^50 + 0.1))
  expect_false(are_integerish(-2^49 + 0.05))
  expect_false(are_integerish(-2^40 + 0.0001))
})

test_that("are_string() matches on string", {
  expect_true(are_string("foo"))
  expect_true(are_string("foo", .string = "foo"))
  expect_false(are_string("foo", .string = "bar"))
  expect_false(are_string(NA, .string = NA))

  expect_true(are_string("foo", .string = list(c("foo", "bar"))))
  expect_true(are_string("foo", .string = list(c("bar", "foo"))))
  expect_false(are_string("foo", .string = list(c("bar", "baz"))))
})


test_that("are_bool() checks for single `TRUE` or `FALSE`", {
  expect_true(are_bool(TRUE))
  expect_true(are_bool(FALSE))
  expect_false(are_bool(NA))
  expect_false(are_bool(c(TRUE, FALSE)))
})
