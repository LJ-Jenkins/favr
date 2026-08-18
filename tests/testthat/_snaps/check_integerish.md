# finite and allow_null work correctly

    Code
      check_integerish(c(1L, 2L, NA_integer_), finite = TRUE)
    Condition
      Error:
      ! `c(1L, 2L, NA_integer_)` must not contain non-finite values.
    Code
      check_integerish(NULL, allow_null = FALSE)
    Condition
      Error:
      ! `NULL` must be an <integer>'ish' vector, not <NULL>.
    Code
      check_scalar_integerish(NA_integer_, finite = TRUE)
    Condition
      Error:
      ! `NA_integer_` must be a finite value, not NA.
    Code
      check_scalar_integerish(NULL, allow_null = FALSE)
    Condition
      Error:
      ! `NULL` must be scalar <integer>'ish', not <NULL>.

# error shows type problem preferentially

    Code
      check_integerish(1.1, n = 2)
    Condition
      Error:
      ! `1.1` must be an <integer>'ish' vector, not the number 1.1.
    Code
      check_integerish(bare(structure(1L, class = "my_integerish")), n = 2)
    Condition
      Error:
      ! `structure(1L, class = "my_integerish")` must be a bare <integer>, but it is of class <my_integerish>.
    Code
      check_integerish(c(1.1, 2.2), n = 1)
    Condition
      Error:
      ! `c(1.1, 2.2)` must be an <integer>'ish' vector, not a <double> vector.
    Code
      check_scalar_integerish(c(1.1, 2.2))
    Condition
      Error:
      ! `c(1.1, 2.2)` must be scalar <integer>'ish', not a <double> vector.
    Code
      check_scalar_integerish(bare(structure(c(1L, 2L), class = "my_integerish")))
    Condition
      Error:
      ! `structure(c(1L, 2L), class = "my_integerish")` must be a bare <integer>, but it is of class <my_integerish>.

# error shows length problem when types match

    Code
      check_integerish(c(1L, 2L), n = 1)
    Condition
      Error:
      ! `c(1L, 2L)` must be an <integer>'ish' vector of length 1, not 2.
    Code
      check_scalar_integerish(c(1L, 2L))
    Condition
      Error:
      ! `c(1L, 2L)` must be scalar <integer>'ish', but it is of length 2.

# arg is shown in error

    Code
      x <- 1.1
      check_integerish(x)
    Condition
      Error:
      ! `x` must be an <integer>'ish' vector, not the number 1.1.
    Code
      check_scalar_integerish(x)
    Condition
      Error:
      ! `x` must be scalar <integer>'ish', not the number 1.1.
    Code
      check_integerish(x, n = 2, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be an <integer>'ish' vector, not the number 1.1.
    Code
      check_scalar_integerish(x, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be scalar <integer>'ish', not the number 1.1.

# call is shown in error

    Code
      f <- (function() {
        check_integerish(1.1)
      })
      fs <- (function() {
        check_scalar_integerish(1.1)
      })
      f()
    Condition
      Error in `f()`:
      ! `1.1` must be an <integer>'ish' vector, not the number 1.1.
    Code
      fs()
    Condition
      Error in `fs()`:
      ! `1.1` must be scalar <integer>'ish', not the number 1.1.

# dots passed to cli_abort/abort

    Code
      check_integerish(1.1, footer = "Custom footer")
    Condition
      Error:
      ! `1.1` must be an <integer>'ish' vector, not the number 1.1.
      Custom footer
    Code
      check_scalar_integerish(1.1, footer = "Custom footer")
    Condition
      Error:
      ! `1.1` must be scalar <integer>'ish', not the number 1.1.
      Custom footer

# .envir doesn't interfere

    Code
      e <- environment()
      e$arg <- "my_arg"
      check_integerish(1.1, .envir = e)
    Condition
      Error:
      ! `1.1` must be an <integer>'ish' vector, not the number 1.1.
    Code
      check_scalar_integerish(1.1, .envir = e)
    Condition
      Error:
      ! `1.1` must be scalar <integer>'ish', not the number 1.1.

