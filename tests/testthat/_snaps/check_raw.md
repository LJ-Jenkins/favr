# allow_null works correctly

    Code
      check_raw(NULL, allow_null = FALSE)
    Condition
      Error:
      ! `NULL` must be a <raw> vector, not <NULL>.
    Code
      check_scalar_raw(NULL, allow_null = FALSE)
    Condition
      Error:
      ! `NULL` must be a scalar <raw>, not <NULL>.

# error shows type problem preferentially

    Code
      check_raw(1.1, n = 2)
    Condition
      Error:
      ! `1.1` must be a <raw> vector, not the number 1.1.
    Code
      check_raw(c(1.1, 2L), n = 1)
    Condition
      Error:
      ! `c(1.1, 2L)` must be a <raw> vector, not a <double> vector.
    Code
      check_raw(bare(structure(raw(1), class = "my_class")), n = 2)
    Condition
      Error:
      ! `structure(raw(1), class = "my_class")` must be a bare <raw>, but it is of class <my_class>.
    Code
      check_scalar_raw(c(1.1, 2L))
    Condition
      Error:
      ! `c(1.1, 2L)` must be a scalar <raw>, not a <double> vector.
    Code
      check_scalar_raw(bare(structure(raw(2), class = "my_class")))
    Condition
      Error:
      ! `structure(raw(2), class = "my_class")` must be a bare <raw>, but it is of class <my_class>.

# error shows length problem when types match

    Code
      check_raw(raw(2), n = 1)
    Condition
      Error:
      ! `raw(2)` must be a <raw> vector of length 1, not 2.
    Code
      check_scalar_raw(raw(2))
    Condition
      Error:
      ! `raw(2)` must be a scalar <raw>, but it is of length 2.

# arg is shown in error

    Code
      x <- 1.1
      check_raw(x)
    Condition
      Error:
      ! `x` must be a <raw> vector, not the number 1.1.
    Code
      check_scalar_raw(x)
    Condition
      Error:
      ! `x` must be a scalar <raw>, not the number 1.1.
    Code
      check_raw(x, n = 2, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be a <raw> vector, not the number 1.1.
    Code
      check_scalar_raw(x, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be a scalar <raw>, not the number 1.1.

# call is shown in error

    Code
      f <- (function() {
        check_raw(1.1)
      })
      fs <- (function() {
        check_scalar_raw(1.1)
      })
      f()
    Condition
      Error in `f()`:
      ! `1.1` must be a <raw> vector, not the number 1.1.
    Code
      fs()
    Condition
      Error in `fs()`:
      ! `1.1` must be a scalar <raw>, not the number 1.1.

# dots passed to cli_abort/abort

    Code
      check_raw(1.1, footer = "Custom footer")
    Condition
      Error:
      ! `1.1` must be a <raw> vector, not the number 1.1.
      Custom footer
    Code
      check_scalar_raw(1.1, footer = "Custom footer")
    Condition
      Error:
      ! `1.1` must be a scalar <raw>, not the number 1.1.
      Custom footer

# .envir doesn't interfere

    Code
      e <- environment()
      e$arg <- "my_arg"
      check_raw(1.1, .envir = e)
    Condition
      Error:
      ! `1.1` must be a <raw> vector, not the number 1.1.
    Code
      check_scalar_raw(1.1, .envir = e)
    Condition
      Error:
      ! `1.1` must be a scalar <raw>, not the number 1.1.

