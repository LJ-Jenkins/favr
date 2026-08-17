# allow_na and allow_null work correctly

    Code
      check_integer(c(1L, 2L, NA_integer_), allow_na = FALSE)
    Condition
      Error:
      ! `c(1L, 2L, NA_integer_)` must not contain NA values.
    Code
      check_integer(NULL, allow_null = FALSE)
    Condition
      Error:
      ! `NULL` must be an <integer> vector, not <NULL>.
    Code
      check_scalar_integer(NA_integer_, allow_na = FALSE)
    Condition
      Error:
      ! `NA_integer_` must not be NA.
    Code
      check_scalar_integer(NULL, allow_null = FALSE)
    Condition
      Error:
      ! `NULL` must be a scalar <integer>, not <NULL>.

# error shows type problem preferentially

    Code
      check_integer(1.1, n = 2)
    Condition
      Error:
      ! `1.1` must be an <integer> vector, not the number 1.1.
    Code
      check_integer(bare(structure(1L, class = "my_integer")), n = 2)
    Condition
      Error:
      ! `structure(1L, class = "my_integer")` must be a bare <integer>, but it is of class <my_integer>.
    Code
      check_integer(c(1.1, 2.2), n = 1)
    Condition
      Error:
      ! `c(1.1, 2.2)` must be an <integer> vector, not a <double> vector.
    Code
      check_scalar_integer(c(1.1, 2.2))
    Condition
      Error:
      ! `c(1.1, 2.2)` must be a scalar <integer>, not a <double> vector.
    Code
      check_scalar_integer(bare(structure(c(1L, 2L), class = "my_integer")))
    Condition
      Error:
      ! `structure(c(1L, 2L), class = "my_integer")` must be a bare <integer>, but it is of class <my_integer>.

# error shows length problem when types match

    Code
      check_integer(c(1L, 2L), n = 1)
    Condition
      Error:
      ! `c(1L, 2L)` must be an <integer> vector of length 1, not 2.
    Code
      check_scalar_integer(c(1L, 2L))
    Condition
      Error:
      ! `c(1L, 2L)` must be a scalar <integer>, but it is of length 2.

# arg is shown in error

    Code
      x <- 1.1
      check_integer(x)
    Condition
      Error:
      ! `x` must be an <integer> vector, not the number 1.1.
    Code
      check_scalar_integer(x)
    Condition
      Error:
      ! `x` must be a scalar <integer>, not the number 1.1.
    Code
      check_integer(x, n = 2, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be an <integer> vector, not the number 1.1.
    Code
      check_scalar_integer(x, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be a scalar <integer>, not the number 1.1.

# call is shown in error

    Code
      f <- (function() {
        check_integer(1.1)
      })
      fs <- (function() {
        check_scalar_integer(1.1)
      })
      f()
    Condition
      Error in `f()`:
      ! `1.1` must be an <integer> vector, not the number 1.1.
    Code
      fs()
    Condition
      Error in `fs()`:
      ! `1.1` must be a scalar <integer>, not the number 1.1.

# dots passed to cli_abort/abort

    Code
      check_integer(1.1, footer = "Custom footer")
    Condition
      Error:
      ! `1.1` must be an <integer> vector, not the number 1.1.
      Custom footer
    Code
      check_scalar_integer(1.1, footer = "Custom footer")
    Condition
      Error:
      ! `1.1` must be a scalar <integer>, not the number 1.1.
      Custom footer

# .envir doesn't interfere

    Code
      e <- environment()
      e$arg <- "my_arg"
      check_integer(1.1, .envir = e)
    Condition
      Error:
      ! `1.1` must be an <integer> vector, not the number 1.1.
    Code
      check_scalar_integer(1.1, .envir = e)
    Condition
      Error:
      ! `1.1` must be a scalar <integer>, not the number 1.1.

