# allow_null works correctly

    Code
      check_vector(NULL, allow_null = FALSE)
    Condition
      Error:
      ! `NULL` must be a <vector>, not <NULL>.
    Code
      check_scalar_vector(NULL, allow_null = FALSE)
    Condition
      Error:
      ! `NULL` must be a scalar <vector>, not <NULL>.

# error shows type problem preferentially

    Code
      e <- new.env()
      e$x <- list(1)
      e$y <- 1
      check_vector(e, n = 2)
    Condition
      Error:
      ! `e` must be a <vector>, not an <environment>.
    Code
      check_vector(e, n = 1)
    Condition
      Error:
      ! `e` must be a <vector>, not an <environment>.
    Code
      check_vector(bare(structure(1, class = "my_class")), n = 2)
    Condition
      Error:
      ! `structure(1, class = "my_class")` must be a bare <vector>, but it is of class <my_class>.
    Code
      check_scalar_vector(e)
    Condition
      Error:
      ! `e` must be a scalar <vector>, not an <environment>.
    Code
      check_scalar_vector(bare(structure(c(1, 2), class = "my_class")))
    Condition
      Error:
      ! `structure(c(1, 2), class = "my_class")` must be a bare <vector>, but it is of class <my_class>.

# error shows length problem when types match

    Code
      check_vector(c(TRUE, FALSE), n = 1)
    Condition
      Error:
      ! `c(TRUE, FALSE)` must be a <vector> of length 1, not 2.
    Code
      check_scalar_vector(c(TRUE, FALSE))
    Condition
      Error:
      ! `c(TRUE, FALSE)` must be a scalar <vector>, but it is of length 2.

# arg is shown in error

    Code
      x <- quote(a + b)
      check_vector(x)
    Condition
      Error:
      ! `x` must be a <vector>, not a <call>.
    Code
      check_scalar_vector(x)
    Condition
      Error:
      ! `x` must be a scalar <vector>, not a <call>.
    Code
      check_vector(x, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be a <vector>, not a <call>.
    Code
      check_scalar_vector(x, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be a scalar <vector>, not a <call>.

# call is shown in error

    Code
      f <- (function() {
        check_vector(mean)
      })
      fs <- (function() {
        check_scalar_vector(mean)
      })
      f()
    Condition
      Error in `f()`:
      ! `mean` must be a <vector>, not a <function>.
    Code
      fs()
    Condition
      Error in `fs()`:
      ! `mean` must be a scalar <vector>, not a <function>.

# dots passed to cli_abort/abort

    Code
      check_vector(mean, footer = "Custom footer")
    Condition
      Error:
      ! `mean` must be a <vector>, not a <function>.
      Custom footer
    Code
      check_scalar_vector(mean, footer = "Custom footer")
    Condition
      Error:
      ! `mean` must be a scalar <vector>, not a <function>.
      Custom footer

# .envir doesn't interfere

    Code
      e <- environment()
      e$arg <- "my_arg"
      check_vector(mean, .envir = e)
    Condition
      Error:
      ! `mean` must be a <vector>, not a <function>.
    Code
      check_scalar_vector(mean, .envir = e)
    Condition
      Error:
      ! `mean` must be a scalar <vector>, not a <function>.

