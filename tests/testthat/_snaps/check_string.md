# allow_empty arg works correctly

    Code
      check_string("", allow_empty = FALSE)
    Condition
      Error:
      ! `""` must not be an empty string.

# string arg works correctly

    Code
      check_string("b", string = "a")
    Condition
      Error:
      ! `"b"` must be one of "a".
    Code
      check_string("z", string = letters[1:5])
    Condition
      Error:
      ! `"z"` must be one of "a", "b", "c", "d", or "e".

# error shows type problem preferentially

    Code
      check_string(1L)
    Condition
      Error:
      ! `1L` must be a single string, not the number 1.
    Code
      check_string(c(1L, 2L))
    Condition
      Error:
      ! `c(1L, 2L)` must be a single string, not an <integer> vector.

# error shows length problem when types match

    Code
      check_string(c("a", "b"))
    Condition
      Error:
      ! `c("a", "b")` must be a single string, not a <character> vector of length 2.

# arg is shown in error

    Code
      x <- 1L
      check_string(x)
    Condition
      Error:
      ! `x` must be a single string, not the number 1.
    Code
      check_string(x, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be a single string, not the number 1.

# call is shown in error

    Code
      f <- (function() {
        check_string(1L)
      })
      f()
    Condition
      Error in `f()`:
      ! `1L` must be a single string, not the number 1.

# dots passed to cli_abort/abort

    Code
      check_string(1L, footer = "Custom footer")
    Condition
      Error:
      ! `1L` must be a single string, not the number 1.
      Custom footer

# .envir doesn't interfere

    Code
      e <- environment()
      e$arg <- "my_arg"
      check_string(1L, .envir = e)
    Condition
      Error:
      ! `1L` must be a single string, not the number 1.

