# allow_null workss correctly

    Code
      check_bytes(NULL, allow_null = FALSE)
    Condition
      Error:
      ! `NULL` must be a <bytes> vector, not <NULL>.
    Code
      check_scalar_bytes(NULL, allow_null = FALSE)
    Condition
      Error:
      ! `NULL` must be a scalar <bytes>, not <NULL>.

# error shows type problem preferentially

    Code
      check_bytes(1.1, n = 2)
    Condition
      Error:
      ! `1.1` must be a <bytes> vector, not the number 1.1.
    Code
      check_bytes(bare(structure(bytes(1), class = "my_bytes")), n = 2)
    Condition
      Error:
      ! `structure(bytes(1), class = "my_bytes")` must be a bare <bytes>, but it is of class <my_bytes>.
    Code
      check_bytes(c(1.1, 2L), n = 1)
    Condition
      Error:
      ! `c(1.1, 2L)` must be a <bytes> vector, not a <double> vector.
    Code
      check_scalar_bytes(c(1.1, 2L))
    Condition
      Error:
      ! `c(1.1, 2L)` must be a scalar <bytes>, not a <double> vector.
    Code
      check_scalar_bytes(bare(structure(bytes(1, 2), class = "my_bytes")))
    Condition
      Error:
      ! `structure(bytes(1, 2), class = "my_bytes")` must be a bare <bytes>, but it is of class <my_bytes>.

# error shows length problem when types match

    Code
      check_bytes(bytes(1, 2), n = 1)
    Condition
      Error:
      ! `bytes(1, 2)` must be a <bytes> vector of length 1, not 2.
    Code
      check_scalar_bytes(bytes(1, 2))
    Condition
      Error:
      ! `bytes(1, 2)` must be a scalar <bytes>, but it is of length 2.

# arg is shown in error

    Code
      x <- 1.1
      check_bytes(x)
    Condition
      Error:
      ! `x` must be a <bytes> vector, not the number 1.1.
    Code
      check_scalar_bytes(x)
    Condition
      Error:
      ! `x` must be a scalar <bytes>, not the number 1.1.
    Code
      check_bytes(x, n = 2, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be a <bytes> vector, not the number 1.1.
    Code
      check_scalar_bytes(x, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be a scalar <bytes>, not the number 1.1.

# call is shown in error

    Code
      f <- (function() {
        check_bytes(1.1)
      })
      fs <- (function() {
        check_scalar_bytes(1.1)
      })
      f()
    Condition
      Error in `f()`:
      ! `1.1` must be a <bytes> vector, not the number 1.1.
    Code
      fs()
    Condition
      Error in `fs()`:
      ! `1.1` must be a scalar <bytes>, not the number 1.1.

# dots passed to cli_abort/abort

    Code
      check_bytes(1.1, footer = "Custom footer")
    Condition
      Error:
      ! `1.1` must be a <bytes> vector, not the number 1.1.
      Custom footer
    Code
      check_scalar_bytes(1.1, footer = "Custom footer")
    Condition
      Error:
      ! `1.1` must be a scalar <bytes>, not the number 1.1.
      Custom footer

# .envir doesn't interfere

    Code
      e <- environment()
      e$arg <- "my_arg"
      check_bytes(1.1, .envir = e)
    Condition
      Error:
      ! `1.1` must be a <bytes> vector, not the number 1.1.
    Code
      check_scalar_bytes(1.1, .envir = e)
    Condition
      Error:
      ! `1.1` must be a scalar <bytes>, not the number 1.1.

