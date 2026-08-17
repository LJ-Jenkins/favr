# allow_null works correctly

    Code
      check_list(NULL, allow_null = FALSE)
    Condition
      Error:
      ! `NULL` must be a <list>, not <NULL>.
    Code
      check_scalar_list(NULL, allow_null = FALSE)
    Condition
      Error:
      ! `NULL` must be a scalar <list>, not <NULL>.

# error shows type problem preferentially

    Code
      e <- new.env()
      e$x <- list(1)
      e$y <- 1
      check_list(e, n = 2)
    Condition
      Error:
      ! `e` must be a <list>, not an <environment>.
    Code
      check_list(e, n = 1)
    Condition
      Error:
      ! `e` must be a <list>, not an <environment>.
    Code
      check_scalar_list(e)
    Condition
      Error:
      ! `e` must be a scalar <list>, not an <environment>.
    Code
      check_list(bare(data.frame(x = 1)), n = 2)
    Condition
      Error:
      ! `data.frame(x = 1)` must be a bare <list>, but it is of class <data.frame>.
    Code
      check_scalar_list(bare(data.frame(x = 1, y = 2)))
    Condition
      Error:
      ! `data.frame(x = 1, y = 2)` must be a bare <list>, but it is of class <data.frame>.

# error shows length problem when types match

    Code
      check_list(list(1, 2), n = 1)
    Condition
      Error:
      ! `list(1, 2)` must be a <list> of length 1, not 2.
    Code
      check_scalar_list(list(1, 2))
    Condition
      Error:
      ! `list(1, 2)` must be a scalar <list>, but it is of length 2.

# arg is shown in error

    Code
      x <- quote(a + b)
      check_list(x)
    Condition
      Error:
      ! `x` must be a <list>, not a <call>.
    Code
      check_scalar_list(x)
    Condition
      Error:
      ! `x` must be a scalar <list>, not a <call>.
    Code
      check_list(x, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be a <list>, not a <call>.
    Code
      check_scalar_list(x, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be a scalar <list>, not a <call>.

# call is shown in error

    Code
      f <- (function() {
        check_list(mean)
      })
      fs <- (function() {
        check_scalar_list(mean)
      })
      f()
    Condition
      Error in `f()`:
      ! `mean` must be a <list>, not a <function>.
    Code
      fs()
    Condition
      Error in `fs()`:
      ! `mean` must be a scalar <list>, not a <function>.

# dots passed to cli_abort/abort

    Code
      check_list(mean, footer = "Custom footer")
    Condition
      Error:
      ! `mean` must be a <list>, not a <function>.
      Custom footer
    Code
      check_scalar_list(mean, footer = "Custom footer")
    Condition
      Error:
      ! `mean` must be a scalar <list>, not a <function>.
      Custom footer

# .envir doesn't interfere

    Code
      e <- environment()
      e$arg <- "my_arg"
      check_list(mean, .envir = e)
    Condition
      Error:
      ! `mean` must be a <list>, not a <function>.
    Code
      check_scalar_list(mean, .envir = e)
    Condition
      Error:
      ! `mean` must be a scalar <list>, not a <function>.

