# allow_null works correctly

    Code
      check_numeric(NULL, allow_null = FALSE)
    Condition
      Error:
      ! `NULL` must be a <numeric> vector, not <NULL>.
    Code
      check_scalar_numeric(NULL, allow_null = FALSE)
    Condition
      Error:
      ! `NULL` must be a scalar <numeric>, not <NULL>.

# finite works correctly

    Code
      check_numeric(c(1.1, 2.2, Inf), finite = TRUE)
    Condition
      Error:
      ! `c(1.1, 2.2, Inf)` must not contain non-finite values.
    Code
      check_numeric(c(1.1, 2.2, NaN), finite = TRUE)
    Condition
      Error:
      ! `c(1.1, 2.2, NaN)` must not contain non-finite values.
    Code
      check_scalar_numeric(Inf, finite = TRUE)
    Condition
      Error:
      ! `Inf` must be a finite value, not Inf.
    Code
      check_scalar_numeric(NaN, finite = TRUE)
    Condition
      Error:
      ! `NaN` must be a finite value, not NaN.

# error shows type problem preferentially

    Code
      check_numeric(list(1), n = 2)
    Condition
      Error:
      ! `list(1)` must be a <numeric> vector, not a <list>.
    Code
      check_numeric(bare(structure(1.1, class = "my_numeric")), n = 2)
    Condition
      Error:
      ! `structure(1.1, class = "my_numeric")` must be a bare <numeric>, but it is of class <my_numeric>.
    Code
      check_numeric(c("a", "b"), n = 1)
    Condition
      Error:
      ! `c("a", "b")` must be a <numeric> vector, not a <character> vector.
    Code
      check_scalar_numeric(c("a", "b"))
    Condition
      Error:
      ! `c("a", "b")` must be a scalar <numeric>, not a <character> vector.
    Code
      check_scalar_numeric(bare(structure(c(1.1, 2.2), class = "my_numeric")))
    Condition
      Error:
      ! `structure(c(1.1, 2.2), class = "my_numeric")` must be a bare <numeric>, but it is of class <my_numeric>.

# error shows length problem when types match

    Code
      check_numeric(c(1.1, 2.2), n = 1)
    Condition
      Error:
      ! `c(1.1, 2.2)` must be a <numeric> vector of length 1, not 2.
    Code
      check_scalar_numeric(c(1.1, 2.2))
    Condition
      Error:
      ! `c(1.1, 2.2)` must be a scalar <numeric>, but it is of length 2.

# arg is shown in error

    Code
      x <- 1L
      check_numeric(x)
      check_scalar_numeric(x)
      check_numeric(x, n = 2, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be a <numeric> vector of length 2, not 1.
    Code
      check_scalar_numeric(x, arg = "my_arg")

# call is shown in error

    Code
      f <- (function() {
        check_numeric("a")
      })
      fs <- (function() {
        check_scalar_numeric("a")
      })
      f()
    Condition
      Error in `f()`:
      ! `"a"` must be a <numeric> vector, not the string "a".
    Code
      fs()
    Condition
      Error in `fs()`:
      ! `"a"` must be a scalar <numeric>, not the string "a".

# dots passed to cli_abort/abort

    Code
      check_numeric("a", footer = "Custom footer")
    Condition
      Error:
      ! `"a"` must be a <numeric> vector, not the string "a".
      Custom footer
    Code
      check_scalar_numeric("a", footer = "Custom footer")
    Condition
      Error:
      ! `"a"` must be a scalar <numeric>, not the string "a".
      Custom footer

# .envir doesn't interfere

    Code
      e <- environment()
      e$arg <- "my_arg"
      check_numeric("a", .envir = e)
    Condition
      Error:
      ! `"a"` must be a <numeric> vector, not the string "a".
    Code
      check_scalar_numeric("a", .envir = e)
    Condition
      Error:
      ! `"a"` must be a scalar <numeric>, not the string "a".

