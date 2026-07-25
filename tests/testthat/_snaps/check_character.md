# allow_na and allow_null work correctly

    Code
      check_character(c("a", "b", NA_character_), allow_na = FALSE)
    Condition
      Error:
      ! `c("a", "b", NA_character_)` must not contain NA values.
    Code
      check_character(NULL, allow_null = FALSE)
    Condition
      Error:
      ! `NULL` must be a <character> vector, not <NULL>.
    Code
      check_scalar_character(NA_character_, allow_na = FALSE)
    Condition
      Error:
      ! `NA_character_` must not be NA.
    Code
      check_scalar_character(NULL, allow_null = FALSE)
    Condition
      Error:
      ! `NULL` must be a scalar <character>, not <NULL>.

# error shows type problem preferentially

    Code
      check_character(1L, n = 2)
    Condition
      Error:
      ! `1L` must be a <character> vector, not the number 1.
    Code
      check_character(bare(structure("", class = "my_class")), n = 2)
    Condition
      Error:
      ! `structure("", class = "my_class")` must be a bare <character>, but it is of class <my_class>.
    Code
      check_character(c(1L, 2L), n = 1)
    Condition
      Error:
      ! `c(1L, 2L)` must be a <character> vector, not an <integer> vector.
    Code
      check_scalar_character(c(1L, 2L))
    Condition
      Error:
      ! `c(1L, 2L)` must be a scalar <character>, not an <integer> vector.
    Code
      check_scalar_character(bare(structure(c("a", "b"), class = "my_class")))
    Condition
      Error:
      ! `structure(c("a", "b"), class = "my_class")` must be a bare <character>, but it is of class <my_class>.

# error shows length problem when types match

    Code
      check_character(c("a", "b"), n = 1)
    Condition
      Error:
      ! `c("a", "b")` must be a <character> vector of length 1, not 2.
    Code
      check_scalar_character(c("a", "b"))
    Condition
      Error:
      ! `c("a", "b")` must be a scalar <character>, but it is of length 2.

# arg is shown in error

    Code
      x <- 1L
      check_character(x)
    Condition
      Error:
      ! `x` must be a <character> vector, not the number 1.
    Code
      check_scalar_character(x)
    Condition
      Error:
      ! `x` must be a scalar <character>, not the number 1.
    Code
      check_character(x, n = 2, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be a <character> vector, not the number 1.
    Code
      check_scalar_character(x, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be a scalar <character>, not the number 1.

# call is shown in error

    Code
      f <- (function() {
        check_character(1L)
      })
      fs <- (function() {
        check_scalar_character(1L)
      })
      f()
    Condition
      Error in `f()`:
      ! `1L` must be a <character> vector, not the number 1.
    Code
      fs()
    Condition
      Error in `fs()`:
      ! `1L` must be a scalar <character>, not the number 1.

# dots passed to cli_abort/abort

    Code
      check_character(1L, footer = "Custom footer")
    Condition
      Error:
      ! `1L` must be a <character> vector, not the number 1.
      Custom footer
    Code
      check_scalar_character(1L, footer = "Custom footer")
    Condition
      Error:
      ! `1L` must be a scalar <character>, not the number 1.
      Custom footer

# .envir doesn't interfere

    Code
      e <- environment()
      e$arg <- "my_arg"
      check_character(1L, .envir = e)
    Condition
      Error:
      ! `1L` must be a <character> vector, not the number 1.
    Code
      check_scalar_character(1L, .envir = e)
    Condition
      Error:
      ! `1L` must be a scalar <character>, not the number 1.

