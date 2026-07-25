# allow_null works correctly

    Code
      check_double(NULL, allow_null = FALSE)
    Condition
      Error:
      ! `NULL` must be a <double> vector, not <NULL>.
    Code
      check_scalar_double(NULL, allow_null = FALSE)
    Condition
      Error:
      ! `NULL` must be a scalar <double>, not <NULL>.

# finite works correctly

    Code
      check_double(c(1.1, 2.2, Inf), finite = TRUE)
    Condition
      Error:
      ! `c(1.1, 2.2, Inf)` must not contain non-finite values.
    Code
      check_double(c(1.1, 2.2, NaN), finite = TRUE)
    Condition
      Error:
      ! `c(1.1, 2.2, NaN)` must not contain non-finite values.
    Code
      check_scalar_double(Inf, finite = TRUE)
    Condition
      Error:
      ! `Inf` must be a finite value, not Inf.
    Code
      check_scalar_double(NaN, finite = TRUE)
    Condition
      Error:
      ! `NaN` must be a finite value, not NaN.

# error shows type problem preferentially

    Code
      check_double(1L, n = 2)
    Condition
      Error:
      ! `1L` must be a <double> vector, not the number 1.
    Code
      check_double(bare(structure(1.1, class = "my_double")), n = 2)
    Condition
      Error:
      ! `structure(1.1, class = "my_double")` must be a bare <double>, but it is of class <my_double>.
    Code
      check_double(c(1L, 2L), n = 1)
    Condition
      Error:
      ! `c(1L, 2L)` must be a <double> vector, not an <integer> vector.
    Code
      check_scalar_double(c(1L, 2L))
    Condition
      Error:
      ! `c(1L, 2L)` must be a scalar <double>, not an <integer> vector.
    Code
      check_scalar_double(bare(structure(c(1.1, 2.2), class = "my_double")))
    Condition
      Error:
      ! `structure(c(1.1, 2.2), class = "my_double")` must be a bare <double>, but it is of class <my_double>.

# error shows length problem when types match

    Code
      check_double(c(1.1, 2.2), n = 1)
    Condition
      Error:
      ! `c(1.1, 2.2)` must be a <double> vector of length 1, not 2.
    Code
      check_scalar_double(c(1.1, 2.2))
    Condition
      Error:
      ! `c(1.1, 2.2)` must be a scalar <double>, but it is of length 2.

# arg is shown in error

    Code
      x <- 1L
      check_double(x)
    Condition
      Error:
      ! `x` must be a <double> vector, not the number 1.
    Code
      check_scalar_double(x)
    Condition
      Error:
      ! `x` must be a scalar <double>, not the number 1.
    Code
      check_double(x, n = 2, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be a <double> vector, not the number 1.
    Code
      check_scalar_double(x, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be a scalar <double>, not the number 1.

# call is shown in error

    Code
      f <- (function() {
        check_double(1L)
      })
      fs <- (function() {
        check_scalar_double(1L)
      })
      f()
    Condition
      Error in `f()`:
      ! `1L` must be a <double> vector, not the number 1.
    Code
      fs()
    Condition
      Error in `fs()`:
      ! `1L` must be a scalar <double>, not the number 1.

# dots passed to cli_abort/abort

    Code
      check_double(1L, footer = "Custom footer")
    Condition
      Error:
      ! `1L` must be a <double> vector, not the number 1.
      Custom footer
    Code
      check_scalar_double(1L, footer = "Custom footer")
    Condition
      Error:
      ! `1L` must be a scalar <double>, not the number 1.
      Custom footer

# .envir doesn't interfere

    Code
      e <- environment()
      e$arg <- "my_arg"
      check_double(1L, .envir = e)
    Condition
      Error:
      ! `1L` must be a <double> vector, not the number 1.
    Code
      check_scalar_double(1L, .envir = e)
    Condition
      Error:
      ! `1L` must be a scalar <double>, not the number 1.

