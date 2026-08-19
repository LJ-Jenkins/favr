# allow_null works correctly

    Code
      check_array(NULL, allow_null = FALSE)
    Condition
      Error:
      ! `NULL` must be an <array>, not <NULL>.
    Code
      check_matrix(NULL, allow_null = FALSE)
    Condition
      Error:
      ! `NULL` must be a <matrix>, not <NULL>.

# error shows type problem preferentially

    Code
      check_array(1, n = 2)
    Condition
      Error:
      ! `1` must be an <array>, not the number 1.
    Code
      check_matrix(1:2, n = 1)
    Condition
      Error:
      ! `1:2` must be a <matrix>, not an <integer> vector.
    Code
      x <- array(1)
      class(x) <- c("my_array", class(x))
      check_array(bare(x), n = 2)
    Condition
      Error:
      ! `x` must be a bare <array>, but it is of class <my_array/array>.
    Code
      x <- matrix(1)
      class(x) <- c("my_matrix", class(x))
      check_matrix(bare(x), n = 2)
    Condition
      Error:
      ! `x` must be a bare <matrix>, but it is of class <my_matrix/matrix/array>.

# error shows length problem when types match

    Code
      check_array(array(1:2), n = 1)
    Condition
      Error:
      ! `array(1:2)` must be an <array> of length 1, not 2.
    Code
      check_matrix(matrix(1:2), ncol = 2)
    Condition
      Error:
      ! `matrix(1:2)` must be a <matrix> with 2 rows, not 1.

# arg is shown in error

    Code
      x <- quote(a + b)
      check_array(x)
    Condition
      Error:
      ! `x` must be an <array>, not a <call>.
    Code
      check_matrix(x)
    Condition
      Error:
      ! `x` must be a <matrix>, not a <call>.
    Code
      check_array(x, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be an <array>, not a <call>.
    Code
      check_matrix(x, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be a <matrix>, not a <call>.

# call is shown in error

    Code
      f <- (function() {
        check_array(mean)
      })
      fs <- (function() {
        check_matrix(mean)
      })
      f()
    Condition
      Error in `f()`:
      ! `mean` must be an <array>, not a <function>.
    Code
      fs()
    Condition
      Error in `fs()`:
      ! `mean` must be a <matrix>, not a <function>.

# dots passed to cli_abort/abort

    Code
      check_array(mean, footer = "Custom footer")
    Condition
      Error:
      ! `mean` must be an <array>, not a <function>.
      Custom footer
    Code
      check_matrix(mean, footer = "Custom footer")
    Condition
      Error:
      ! `mean` must be a <matrix>, not a <function>.
      Custom footer

# .envir doesn't interfere

    Code
      e <- environment()
      e$arg <- "my_arg"
      check_array(mean, .envir = e)
    Condition
      Error:
      ! `mean` must be an <array>, not a <function>.
    Code
      check_matrix(mean, .envir = e)
    Condition
      Error:
      ! `mean` must be a <matrix>, not a <function>.

