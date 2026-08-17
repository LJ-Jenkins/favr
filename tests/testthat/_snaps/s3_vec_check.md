# n arg checks length correctly

    Code
      s3_vec_check(x, n = 1, "factor")
    Condition
      Error:
      ! `x` must be a <factor> of length 1, not 2.
    Code
      s3_vec_check(x, n = at_least(3), "factor", "a {.cls factor} vector")
    Condition
      Error:
      ! `x` must be a <factor> vector of at least length 3, but it is of length 2.
    Code
      s3_vec_check(x, n = at_most(1), "factor")
    Condition
      Error:
      ! `x` must be a <factor> of at most length 1, but it is of length 2.
    Code
      s3_vec_check(x, n = in_range(3, 5), "factor", "a {.cls factor} vector")
    Condition
      Error:
      ! `x` must be a <factor> vector of a length between 3 and 5, but it is of length 2.

# allow_null works correctly

    Code
      s3_vec_check(NULL, n = NULL, "factor", allow_null = FALSE)
    Condition
      Error:
      ! `NULL` must inherit from <factor>, but is class <NULL>.
    Code
      s3_vec_check(NULL, n = NULL, "factor", "{.cls factor} vector", allow_null = FALSE)
    Condition
      Error:
      ! `NULL` must inherit from <factor>, but is class <NULL>.

# error shows type problem preferentially

    Code
      s3_vec_check(list(1), n = 2, "Date")
    Condition
      Error:
      ! `list(1)` must inherit from <Date>, but is class <list>.
    Code
      s3_vec_check(bare(structure(1.1, class = c("c1", "c2"))), n = 2, "c1")
    Condition
      Error:
      ! `structure(1.1, class = c("c1", "c2"))` must be a <c1> of length 2, not 1.
    Code
      s3_vec_check(c("a", "b"), n = 1, "Date")
    Condition
      Error:
      ! `c("a", "b")` must inherit from <Date>, but is class <character>.

# error shows length problem when types match

    Code
      s3_vec_check(c(1.1, 2.2), n = 1, "Date")
    Condition
      Error:
      ! `c(1.1, 2.2)` must inherit from <Date>, but is class <numeric>.

# arg is shown in error

    Code
      x <- 1L
      s3_vec_check(x, NULL, "factor")
    Condition
      Error:
      ! `x` must inherit from <factor>, but is class <integer>.
    Code
      s3_vec_check(x, n = 2, "factor", arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must inherit from <factor>, but is class <integer>.

# call is shown in error

    Code
      f <- (function() {
        s3_vec_check("a", NULL, "Date")
      })
      f()
    Condition
      Error in `f()`:
      ! `"a"` must inherit from <Date>, but is class <character>.

# dots passed to cli_abort/abort

    Code
      s3_vec_check("a", NULL, "Date", footer = "Custom footer")
    Condition
      Error:
      ! `"a"` must inherit from <Date>, but is class <character>.
      Custom footer

# .envir doesn't interfere

    Code
      e <- environment()
      e$arg <- "my_arg"
      s3_vec_check("a", NULL, "Date", .envir = e)
    Condition
      Error:
      ! `"a"` must inherit from <Date>, but is class <character>.

