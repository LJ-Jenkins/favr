# type not checked - base function show type errors

    Code
      check_no_na(mean)
    Condition
      Error in `na_check()`:
      ! anyNA() applied to non-(list or vector) of type 'closure'
    Code
      check_finite(mean)
    Condition
      Error in `is.finite()`:
      ! default method not implemented for type 'closure'
    Code
      check_nzchar(mean)
    Condition
      Error in `empty_string_check()`:
      ! cannot coerce type 'closure' to vector of type 'character'

# arg is shown in error

    Code
      x <- NA
      check_no_na(x)
    Condition
      Error:
      ! `x` must not be NA.
    Code
      check_no_na(x, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must not be NA.
    Code
      check_finite(x)
    Condition
      Error:
      ! `x` must be a finite value, not NA.
    Code
      check_finite(x, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be a finite value, not NA.
    Code
      x <- ""
      check_nzchar(x)
    Condition
      Error:
      ! `x` must not be an empty string.
    Code
      check_nzchar(x, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must not be an empty string.
    Code
      x <- "  "
      check_nzchar(x, allow_all_ws = FALSE)
    Condition
      Error:
      ! `x` must not be all whitespace.
    Code
      check_nzchar(x, allow_all_ws = FALSE, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must not be all whitespace.

# call is shown in error

    Code
      f <- (function() {
        check_no_na(NA)
      })
      f()
    Condition
      Error in `f()`:
      ! `NA` must not be NA.
    Code
      f <- (function() {
        check_finite(NA)
      })
      f()
    Condition
      Error in `f()`:
      ! `NA` must be a finite value, not NA.
    Code
      f <- (function() {
        check_nzchar("")
      })
      f()
    Condition
      Error in `f()`:
      ! `""` must not be an empty string.
    Code
      f <- (function() {
        check_nzchar(" ", allow_all_ws = FALSE)
      })
      f()
    Condition
      Error in `f()`:
      ! `" "` must not be all whitespace.

# dots passed to cli_abort/abort

    Code
      check_no_na(NA, footer = "Custom footer")
    Condition
      Error:
      ! `NA` must not be NA.
      Custom footer
    Code
      check_finite(NA, footer = "Custom footer")
    Condition
      Error:
      ! `NA` must be a finite value, not NA.
      Custom footer
    Code
      check_nzchar("", footer = "Custom footer")
    Condition
      Error:
      ! `""` must not be an empty string.
      Custom footer
    Code
      check_nzchar(" ", allow_all_ws = FALSE, footer = "Custom footer")
    Condition
      Error:
      ! `" "` must not be all whitespace.
      Custom footer

# .envir doesn't interfere

    Code
      e <- environment()
      e$arg <- "my_arg"
      check_no_na(NA, .envir = e)
    Condition
      Error:
      ! `NA` must not be NA.
    Code
      check_finite(NA, .envir = e)
    Condition
      Error:
      ! `NA` must be a finite value, not NA.
    Code
      check_nzchar("", .envir = e)
    Condition
      Error:
      ! `""` must not be an empty string.
    Code
      check_nzchar(" ", allow_all_ws = FALSE, .envir = e)
    Condition
      Error:
      ! `" "` must not be all whitespace.

