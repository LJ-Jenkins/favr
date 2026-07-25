# allow_na and allow_null work correctly

    Code
      check_logical(c(TRUE, FALSE, NA), allow_na = FALSE)
    Condition
      Error:
      ! `c(TRUE, FALSE, NA)` must not contain NA values.
    Code
      check_logical(NULL, allow_null = FALSE)
    Condition
      Error:
      ! `NULL` must be a <logical> vector, not <NULL>.
    Code
      check_scalar_logical(NA, allow_na = FALSE)
    Condition
      Error:
      ! `NA` must not be NA.
    Code
      check_scalar_logical(NULL, allow_null = FALSE)
    Condition
      Error:
      ! `NULL` must be a scalar <logical>, not <NULL>.

# error shows type problem preferentially

    Code
      check_logical(1, n = 2)
    Condition
      Error:
      ! `1` must be a <logical> vector, not the number 1.
    Code
      check_logical(1:2, n = 1)
    Condition
      Error:
      ! `1:2` must be a <logical> vector, not an <integer> vector.
    Code
      check_logical(structure(TRUE, class = "my_class"), n = 2)
    Condition
      Error:
      ! `structure(TRUE, class = "my_class")` must be a <logical> vector of length 2, not 1.
    Code
      check_scalar_logical(1:2)
    Condition
      Error:
      ! `1:2` must be a scalar <logical>, not an <integer> vector.
    Code
      check_scalar_logical(structure(c(TRUE, FALSE), class = "my_class"))
    Condition
      Error:
      ! `structure(c(TRUE, FALSE), class = "my_class")` must be a scalar <logical>, but it is of length 2.

# error shows length problem when types match

    Code
      check_logical(c(TRUE, FALSE), n = 1)
    Condition
      Error:
      ! `c(TRUE, FALSE)` must be a <logical> vector of length 1, not 2.
    Code
      check_scalar_logical(c(TRUE, FALSE))
    Condition
      Error:
      ! `c(TRUE, FALSE)` must be a scalar <logical>, but it is of length 2.

# arg is shown in error

    Code
      x <- 1
      check_logical(x)
    Condition
      Error:
      ! `x` must be a <logical> vector, not the number 1.
    Code
      check_scalar_logical(x)
    Condition
      Error:
      ! `x` must be a scalar <logical>, not the number 1.
    Code
      check_logical(x, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be a <logical> vector, not the number 1.
    Code
      check_scalar_logical(x, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be a scalar <logical>, not the number 1.

# call is shown in error

    Code
      f <- (function() {
        check_logical(1)
      })
      fs <- (function() {
        check_scalar_logical(1)
      })
      f()
    Condition
      Error in `f()`:
      ! `1` must be a <logical> vector, not the number 1.
    Code
      fs()
    Condition
      Error in `fs()`:
      ! `1` must be a scalar <logical>, not the number 1.

# dots passed to cli_abort/abort

    Code
      check_logical(1, footer = "Custom footer")
    Condition
      Error:
      ! `1` must be a <logical> vector, not the number 1.
      Custom footer
    Code
      check_scalar_logical(1, footer = "Custom footer")
    Condition
      Error:
      ! `1` must be a scalar <logical>, not the number 1.
      Custom footer

# .envir doesn't interfere

    Code
      e <- environment()
      e$arg <- "my_arg"
      check_logical(1, .envir = e)
    Condition
      Error:
      ! `1` must be a <logical> vector, not the number 1.
    Code
      check_scalar_logical(1, .envir = e)
    Condition
      Error:
      ! `1` must be a scalar <logical>, not the number 1.

