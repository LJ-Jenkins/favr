# type not checked - errors from incorrect types

    Code
      check_size(mean)
    Condition
      Error in `check_size()`:
      ! argument "n" is missing, with no default
    Code
      check_nrow(mean)
    Condition
      Error in `check_nrow()`:
      ! argument "nrow" is missing, with no default
    Code
      check_ncol(mean)
    Condition
      Error in `check_ncol()`:
      ! argument "ncol" is missing, with no default

# arg is shown in error

    Code
      x <- 1:2
      check_length(x, 1)
    Condition
      Error:
      ! `x` must be of length 1, not 2.
    Code
      check_length(x, 1, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be of length 1, not 2.
    Code
      check_size(x, 1)
    Condition
      Error:
      ! `x` must be of size 1, not 2.
    Code
      check_size(x, 1, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be of size 1, not 2.
    Code
      x <- data.frame(x = 1:2)
      check_nrow(x, 1)
    Condition
      Error:
      ! `x` must have 1 row, not 2.
    Code
      check_nrow(x, 1, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must have 1 row, not 2.
    Code
      check_ncol(x, 1)
      check_ncol(x, 1, arg = "my_arg")
      x <- NULL
      check_non_empty(x)
    Condition
      Error:
      ! `x` must not be empty.
    Code
      check_non_empty(x, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must not be empty.
    Code
      check_named(x)
    Condition
      Error:
      ! `x` must be named.
    Code
      check_named(x, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be named.

# call is shown in error

    Code
      f <- (function() {
        check_length(1, 2)
      })
      f()
    Condition
      Error in `f()`:
      ! `1` must be of length 2, not 1.
    Code
      f <- (function() {
        check_size(1, 2)
      })
      f()
    Condition
      Error in `f()`:
      ! `1` must be of size 2, not 1.
    Code
      f <- (function() {
        check_nrow(data.frame(x = 1), 2)
      })
      f()
    Condition
      Error in `f()`:
      ! `data.frame(x = 1)` must have 2 rows, not 1.
    Code
      f <- (function() {
        check_ncol(data.frame(x = 1), 2)
      })
      f()
    Condition
      Error in `f()`:
      ! `data.frame(x = 1)` must have 2 rows, not 1.
    Code
      f <- (function() {
        check_non_empty(NULL)
      })
      f()
    Condition
      Error in `f()`:
      ! `NULL` must not be empty.
    Code
      f <- (function() {
        check_named(1:2)
      })
      f()
    Condition
      Error in `f()`:
      ! `1:2` must be named.

# dots passed to cli_abort/abort

    Code
      check_length(1, 2, footer = "Custom footer")
    Condition
      Error:
      ! `1` must be of length 2, not 1.
      Custom footer
    Code
      check_size(1, 2, footer = "Custom footer")
    Condition
      Error:
      ! `1` must be of size 2, not 1.
      Custom footer
    Code
      check_nrow(data.frame(x = 1), 2, footer = "Custom footer")
    Condition
      Error:
      ! `data.frame(x = 1)` must have 2 rows, not 1.
      Custom footer
    Code
      check_ncol(data.frame(x = 1), 2, footer = "Custom footer")
    Condition
      Error:
      ! `data.frame(x = 1)` must have 2 rows, not 1.
      Custom footer
    Code
      check_non_empty(numeric(0), footer = "Custom footer")
    Condition
      Error:
      ! `numeric(0)` must not be empty.
      Custom footer
    Code
      check_named(1:2, footer = "Custom footer")
    Condition
      Error:
      ! `1:2` must be named.
      Custom footer

# .envir doesn't interfere

    Code
      e <- environment()
      e$arg <- "my_arg"
      check_length(1, 2, .envir = e)
    Condition
      Error:
      ! `1` must be of length 2, not 1.
    Code
      check_size(1, 2, .envir = e)
    Condition
      Error:
      ! `1` must be of size 2, not 1.
    Code
      check_nrow(data.frame(x = 1), 2, .envir = e)
    Condition
      Error:
      ! `data.frame(x = 1)` must have 2 rows, not 1.
    Code
      check_ncol(data.frame(x = 1), 2, .envir = e)
    Condition
      Error:
      ! `data.frame(x = 1)` must have 2 rows, not 1.
    Code
      check_non_empty(numeric(0), .envir = e)
    Condition
      Error:
      ! `numeric(0)` must not be empty.
    Code
      check_named(1:2, .envir = e)
    Condition
      Error:
      ! `1:2` must be named.

