# allow_null works correctly

    Code
      check_complex(NULL, allow_null = FALSE)
    Condition
      Error:
      ! `NULL` must be a <complex> vector, not <NULL>.
    Code
      check_scalar_complex(NULL, allow_null = FALSE)
    Condition
      Error:
      ! `NULL` must be a scalar <complex>, not <NULL>.

# finite works correctly

    Code
      check_complex(as.complex(c(1, 2, Inf)), finite = TRUE)
    Condition
      Error:
      ! `as.complex(c(1, 2, Inf))` must not contain non-finite values.
    Code
      check_complex(as.complex(c(1, 2, NaN)), finite = TRUE)
    Condition
      Error:
      ! `as.complex(c(1, 2, NaN))` must not contain non-finite values.
    Code
      check_scalar_complex(as.complex(Inf), finite = TRUE)
    Condition
      Error:
      ! `as.complex(Inf)` must be a finite value, not Inf+0i.
    Code
      check_scalar_complex(as.complex(NaN), finite = TRUE)
    Condition
      Error:
      ! `as.complex(NaN)` must be a finite value, not NaN+0i.

# error shows type problem preferentially

    Code
      check_complex(1.1, n = 2)
    Condition
      Error:
      ! `1.1` must be a <complex> vector, not the number 1.1.
    Code
      check_complex(bare(structure(complex(1), class = "my_complex")), n = 2)
    Condition
      Error:
      ! `structure(complex(1), class = "my_complex")` must be a bare <complex>, but it is of class <my_complex>.
    Code
      check_complex(c(1.1, 2L), n = 1)
    Condition
      Error:
      ! `c(1.1, 2L)` must be a <complex> vector, not a <double> vector.
    Code
      check_scalar_complex(c(1.1, 2L))
    Condition
      Error:
      ! `c(1.1, 2L)` must be a scalar <complex>, not a <double> vector.
    Code
      check_scalar_complex(bare(structure(complex(2), class = "my_complex")))
    Condition
      Error:
      ! `structure(complex(2), class = "my_complex")` must be a bare <complex>, but it is of class <my_complex>.

# error shows length problem when types match

    Code
      check_complex(complex(2), n = 1)
    Condition
      Error:
      ! `complex(2)` must be a <complex> vector of length 1, not 2.
    Code
      check_scalar_complex(complex(2))
    Condition
      Error:
      ! `complex(2)` must be a scalar <complex>, but it is of length 2.

# arg is shown in error

    Code
      x <- 1.1
      check_complex(x)
    Condition
      Error:
      ! `x` must be a <complex> vector, not the number 1.1.
    Code
      check_scalar_complex(x)
    Condition
      Error:
      ! `x` must be a scalar <complex>, not the number 1.1.
    Code
      check_complex(x, n = 2, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be a <complex> vector, not the number 1.1.
    Code
      check_scalar_complex(x, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be a scalar <complex>, not the number 1.1.

# call is shown in error

    Code
      f <- (function() {
        check_complex(1.1)
      })
      fs <- (function() {
        check_scalar_complex(1.1)
      })
      f()
    Condition
      Error in `f()`:
      ! `1.1` must be a <complex> vector, not the number 1.1.
    Code
      fs()
    Condition
      Error in `fs()`:
      ! `1.1` must be a scalar <complex>, not the number 1.1.

# dots passed to cli_abort/abort

    Code
      check_complex(1.1, footer = "Custom footer")
    Condition
      Error:
      ! `1.1` must be a <complex> vector, not the number 1.1.
      Custom footer
    Code
      check_scalar_complex(1.1, footer = "Custom footer")
    Condition
      Error:
      ! `1.1` must be a scalar <complex>, not the number 1.1.
      Custom footer

# .envir doesn't interfere

    Code
      e <- environment()
      e$arg <- "my_arg"
      check_complex(1.1, .envir = e)
    Condition
      Error:
      ! `1.1` must be a <complex> vector, not the number 1.1.
    Code
      check_scalar_complex(1.1, .envir = e)
    Condition
      Error:
      ! `1.1` must be a scalar <complex>, not the number 1.1.

