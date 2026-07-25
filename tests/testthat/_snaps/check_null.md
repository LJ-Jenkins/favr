# error shows type problem

    Code
      check_null(1L)
    Condition
      Error:
      ! `1L` must be <NULL>, not the number 1.
    Code
      check_null(c(1L, 2L))
    Condition
      Error:
      ! `c(1L, 2L)` must be <NULL>, not an <integer> vector.

# arg is shown in error

    Code
      x <- 1L
      check_null(x)
    Condition
      Error:
      ! `x` must be <NULL>, not the number 1.
    Code
      check_null(x, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be <NULL>, not the number 1.

# call is shown in error

    Code
      f <- (function() {
        check_null(1L)
      })
      f()
    Condition
      Error in `f()`:
      ! `1L` must be <NULL>, not the number 1.

# dots passed to cli_abort/abort

    Code
      check_null(1L, footer = "Custom footer")
    Condition
      Error:
      ! `1L` must be <NULL>, not the number 1.
      Custom footer

# .envir doesn't interfere

    Code
      e <- environment()
      e$arg <- "my_arg"
      check_null(1L, .envir = e)
    Condition
      Error:
      ! `1L` must be <NULL>, not the number 1.

