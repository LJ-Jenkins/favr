# error shows type problem

    Code
      check_true(1L)
    Condition
      Error:
      ! `1L` must be a single TRUE, not the number 1.
    Code
      check_true(c(1L, 2L))
    Condition
      Error:
      ! `c(1L, 2L)` must be a single TRUE, not an <integer> vector.
    Code
      check_false(1L)
    Condition
      Error:
      ! `1L` must be a single FALSE, not the number 1.
    Code
      check_false(c(1L, 2L))
    Condition
      Error:
      ! `c(1L, 2L)` must be a single FALSE, not an <integer> vector.

# arg is shown in error

    Code
      x <- 1L
      check_true(x)
    Condition
      Error:
      ! `x` must be a single TRUE, not the number 1.
    Code
      check_true(x, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be a single TRUE, not the number 1.
    Code
      check_false(x)
    Condition
      Error:
      ! `x` must be a single FALSE, not the number 1.
    Code
      check_false(x, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be a single FALSE, not the number 1.

# call is shown in error

    Code
      f <- (function() {
        check_true(1L)
      })
      f()
    Condition
      Error in `f()`:
      ! `1L` must be a single TRUE, not the number 1.
    Code
      f <- (function() {
        check_false(1L)
      })
      f()
    Condition
      Error in `f()`:
      ! `1L` must be a single FALSE, not the number 1.

# dots passed to cli_abort/abort

    Code
      check_true(1L, footer = "Custom footer")
    Condition
      Error:
      ! `1L` must be a single TRUE, not the number 1.
      Custom footer
    Code
      check_false(1L, footer = "Custom footer")
    Condition
      Error:
      ! `1L` must be a single FALSE, not the number 1.
      Custom footer

# .envir doesn't interfere

    Code
      e <- environment()
      e$arg <- "my_arg"
      check_true(1L, .envir = e)
    Condition
      Error:
      ! `1L` must be a single TRUE, not the number 1.
    Code
      check_false(1L, .envir = e)
    Condition
      Error:
      ! `1L` must be a single FALSE, not the number 1.

