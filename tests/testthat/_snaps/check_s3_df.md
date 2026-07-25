# allow_null works correctly

    Code
      check_s3_df(NULL, nrow = NULL, ncol = NULL, allow_null = FALSE, type = "data.frame")
    Condition
      Error:
      ! `NULL` must inherit from <data.frame>, but is class <NULL>.
    Code
      check_s3_df(NULL, nrow = NULL, ncol = NULL, allow_null = FALSE, type = "data.frame",
        "{.cls data.frame} obj")
    Condition
      Error:
      ! `NULL` must inherit from <data.frame>, but is class <NULL>.

# error shows type problem preferentially

    Code
      check_s3_df(list(1), nrow = 2, ncol = NULL, type = "data.frame")
    Condition
      Error:
      ! `list(1)` must inherit from <data.frame>, but is class <list>.
    Code
      check_s3_df(bare(structure(1.1, class = c("c1", "c2"))), nrow = 2, ncol = NULL,
      type = "c1")
    Condition
      Error:
      ! `structure(1.1, class = c("c1", "c2"))` must be a <c1> with 2 rows, not 1.
    Code
      check_s3_df(c("a", "b"), nrow = 1, ncol = NULL, type = "data.frame")
    Condition
      Error:
      ! `c("a", "b")` must inherit from <data.frame>, but is class <character>.

# error shows length problem when types match

    Code
      check_s3_df(data.frame(x = 1:2), nrow = 1, ncol = NULL, type = "data.frame")
    Condition
      Error:
      ! `data.frame(x = 1:2)` must be a <data.frame> with 1 row, not 2.
    Code
      check_s3_df(data.frame(x = 1:2), nrow = NULL, ncol = 2, type = "data.frame")
    Condition
      Error:
      ! `data.frame(x = 1:2)` must be a <data.frame> with 2 rows, not 1.

# arg is shown in error

    Code
      x <- 1L
      check_s3_df(x, nrow = NULL, ncol = NULL, type = "data.frame")
    Condition
      Error:
      ! `x` must inherit from <data.frame>, but is class <integer>.
    Code
      check_s3_df(x, nrow = 2, ncol = NULL, type = "data.frame", arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must inherit from <data.frame>, but is class <integer>.

# call is shown in error

    Code
      f <- (function() {
        check_s3_df("a", nrow = NULL, ncol = NULL, type = "data.frame")
      })
      f()
    Condition
      Error in `f()`:
      ! `"a"` must inherit from <data.frame>, but is class <character>.

# dots passed to cli_abort/abort

    Code
      check_s3_df("a", nrow = NULL, ncol = NULL, type = "data.frame", footer = "Custom footer")
    Condition
      Error:
      ! `"a"` must inherit from <data.frame>, but is class <character>.
      Custom footer

# .envir doesn't interfere

    Code
      e <- environment()
      e$arg <- "my_arg"
      check_s3_df("a", nrow = NULL, ncol = NULL, type = "data.frame", .envir = e)
    Condition
      Error:
      ! `"a"` must inherit from <data.frame>, but is class <character>.

