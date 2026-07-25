# FIX ALL

test_that("check_inherits() exact matching", {
  expect_null(check_inherits(1L, "integer", match = "exact"))
  expect_error(check_inherits(1L, "double", match = "exact"))
  expect_error(check_inherits(1L, c("integer", "double"), match = "exact"))
  expect_snapshot(error = TRUE, {
    a <- structure(1, class = c("a", "b"))
    check_inherits(a, c("a", "c"), match = "exact")
  })
})

test_that("check_class() exact matching", {
  expect_null(check_class(1L, "integer"))
  expect_error(check_class(1L, "double"))
  expect_error(check_class(1L, c("integer", "double")))
  expect_snapshot(error = TRUE, {
    a <- structure(1, class = c("a", "b"))
    check_class(a, c("a", "c"))
  })
})

test_that("check_inherits() any matching", {
  x <- structure(1, class = c("a", "b", "c"))
  expect_null(check_inherits(x, c("a", "x", "z"), match = "any"))
  expect_error(check_inherits(x, c("x", "y", "z"), match = "any"))
  expect_snapshot(error = TRUE, {
    a <- structure(1, class = c("a", "b", "c"))
    check_inherits(a, c("d", "e", "f"), match = "any")
  })
})

test_that("check_inherits() all matching", {
  x <- structure(1, class = c("a", "b", "c", "d"))
  expect_null(check_inherits(x, c("b", "c"), match = "all"))
  expect_error(check_inherits(x, c("b", "z"), match = "all"))
  expect_snapshot(error = TRUE, {
    check_inherits(x, c("b", "z"), match = "all")
  })
})

test_that("check_inherits() errors on invalid match", {
  expect_error(check_inherits(1, "a", match = "bad"))
})

test_that("check_inherits() errors when class is not character", {
  expect_error(check_inherits(1, 1))
  expect_error(check_inherits(1, list("a")))
  expect_error(check_inherits(1, factor("a")))
  # maybe disallow this in future
  expect_null(
    check_inherits(structure(1L, class = NA_character_), NA_character_)
  )
  expect_error(check_inherits(1, NULL))
})

test_that("check_inherits() uses arg in error messages", {
  x <- 1L
  expect_snapshot(error = TRUE, {
    check_inherits(x, 1)
    check_inherits(x, 1, arg = "my_x")
  })
})

test_that("check_inherits() forwards args to cli_abort correctly", {
  expect_snapshot(error = TRUE, {
    check_inherits(1L, 1, footer = "Custom footer")
  })
})

test_that(".envir doesn't interfere with error messages", {
  expect_snapshot(error = TRUE, {
    e <- environment()
    e$arg <- "my_arg"
    e$target <- "woops chr class"
    e$x <- "woops chr class"
    check_inherits(1L, "double", .envir = e)
  })

  expect_snapshot(error = TRUE, {
    e <- environment()
    e$arg <- "my_arg"
    e$target <- "woops chr class"
    e$x <- "woops chr class"
    check_class(1L, "double", .envir = e)
  })
})
