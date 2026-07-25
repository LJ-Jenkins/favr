# allow_na and allow_null works correctly

    Code
      check_atomic(c(TRUE, FALSE, NA), allow_na = FALSE)
    Condition
      Error:
      ! `c(TRUE, FALSE, NA)` must not contain NA values.
    Code
      check_atomic(NULL, allow_null = FALSE)
    Condition
      Error:
      ! `NULL` must be an <atomic> vector, not <NULL>.
    Code
      check_scalar_atomic(NA, allow_na = FALSE)
    Condition
      Error:
      ! `NA` must not be NA.
    Code
      check_scalar_atomic(NULL, allow_null = FALSE)
    Condition
      Error:
      ! `NULL` must be a scalar <atomic>, not <NULL>.

# error shows type problem preferentially

    Code
      check_atomic(list(1), n = 2)
    Condition
      Error:
      ! `list(1)` must be an <atomic> vector, not a <list>.
    Code
      check_atomic(bare(structure(1, class = "my_class")), n = 2)
    Condition
      Error:
      ! `structure(1, class = "my_class")` must be a bare <atomic>, but it is of class <my_class>.
    Code
      check_atomic(list(1, 2), n = 1)
    Condition
      Error:
      ! `list(1, 2)` must be an <atomic> vector, not a <list>.
    Code
      check_scalar_atomic(list(1, 2))
    Condition
      Error:
      ! `list(1, 2)` must be a scalar <atomic>, not a <list>.
    Code
      check_scalar_atomic(bare(structure(c(TRUE, FALSE), class = "my_class")))
    Condition
      Error:
      ! `structure(c(TRUE, FALSE), class = "my_class")` must be a bare <atomic>, but it is of class <my_class>.

# error shows length problem when types match

    Code
      check_atomic(c(TRUE, FALSE), n = 1)
    Condition
      Error:
      ! `c(TRUE, FALSE)` must be an <atomic> vector of length 1, not 2.
    Code
      check_scalar_atomic(c(TRUE, FALSE))
    Condition
      Error:
      ! `c(TRUE, FALSE)` must be a scalar <atomic>, but it is of length 2.

# arg is shown in error

    Code
      x <- list(1)
      check_atomic(x)
    Condition
      Error:
      ! `x` must be an <atomic> vector, not a <list>.
    Code
      check_scalar_atomic(x)
    Condition
      Error:
      ! `x` must be a scalar <atomic>, not a <list>.
    Code
      check_atomic(x, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be an <atomic> vector, not a <list>.
    Code
      check_scalar_atomic(x, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be a scalar <atomic>, not a <list>.

# call is shown in error

    Code
      f <- (function() {
        check_atomic(list(1))
      })
      fs <- (function() {
        check_scalar_atomic(list(1))
      })
      f()
    Condition
      Error in `f()`:
      ! `list(1)` must be an <atomic> vector, not a <list>.
    Code
      fs()
    Condition
      Error in `fs()`:
      ! `list(1)` must be a scalar <atomic>, not a <list>.

# dots passed to cli_abort/abort

    Code
      check_atomic(list(1), footer = "Custom footer")
    Condition
      Error:
      ! `list(1)` must be an <atomic> vector, not a <list>.
      Custom footer
    Code
      check_scalar_atomic(list(1), footer = "Custom footer")
    Condition
      Error:
      ! `list(1)` must be a scalar <atomic>, not a <list>.
      Custom footer

# .envir doesn't interfere

    Code
      e <- environment()
      e$arg <- "my_arg"
      check_atomic(list(1), .envir = e)
    Condition
      Error:
      ! `list(1)` must be an <atomic> vector, not a <list>.
    Code
      check_scalar_atomic(list(1), .envir = e)
    Condition
      Error:
      ! `list(1)` must be a scalar <atomic>, not a <list>.

