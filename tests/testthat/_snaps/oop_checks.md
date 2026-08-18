# arg is shown in error

    Code
      x <- 1:2
      check_s3(x)
    Condition
      Error:
      ! `x` must be an <S3> object, not <integer>.
    Code
      check_s3(x, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be an <S3> object, not <integer>.
    Code
      check_s4(x)
    Condition
      Error:
      ! `x` must be an <S4> object, not <integer>.
    Code
      check_s4(x, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be an <S4> object, not <integer>.
    Code
      check_s7(x)
    Condition
      Error:
      ! `x` must inherit from <S7_object>, but is class <integer>.
    Code
      check_s7(x, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must inherit from <S7_object>, but is class <integer>.
    Code
      check_r6(x)
    Condition
      Error:
      ! `x` must inherit from <R6>, but is class <integer>.
    Code
      check_r6(x, arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must inherit from <R6>, but is class <integer>.

# call is shown in error

    Code
      f <- (function() {
        check_s3(1)
      })
      f()
    Condition
      Error in `f()`:
      ! `1` must be an <S3> object, not <numeric>.
    Code
      f <- (function() {
        check_s4(1)
      })
      f()
    Condition
      Error in `f()`:
      ! `1` must be an <S4> object, not <numeric>.
    Code
      f <- (function() {
        check_s7(1)
      })
      f()
    Condition
      Error in `f()`:
      ! `1` must inherit from <S7_object>, but is class <numeric>.
    Code
      f <- (function() {
        check_r6(1)
      })
      f()
    Condition
      Error in `f()`:
      ! `1` must inherit from <R6>, but is class <numeric>.

# dots passed to cli_abort/abort

    Code
      check_s3(1, footer = "Custom footer")
    Condition
      Error:
      ! `1` must be an <S3> object, not <numeric>.
      Custom footer
    Code
      check_s4(1, footer = "Custom footer")
    Condition
      Error:
      ! `1` must be an <S4> object, not <numeric>.
      Custom footer
    Code
      check_s7(1, footer = "Custom footer")
    Condition
      Error:
      ! `1` must inherit from <S7_object>, but is class <numeric>.
      Custom footer
    Code
      check_r6(1, footer = "Custom footer")
    Condition
      Error:
      ! `1` must inherit from <R6>, but is class <numeric>.
      Custom footer

# .envir doesn't interfere

    Code
      e <- environment()
      e$arg <- "my_arg"
      check_s3(1, .envir = e)
    Condition
      Error:
      ! `1` must be an <S3> object, not <numeric>.
    Code
      check_s4(1, .envir = e)
    Condition
      Error:
      ! `1` must be an <S4> object, not <numeric>.
    Code
      check_s7(1, .envir = e)
    Condition
      Error:
      ! `1` must inherit from <S7_object>, but is class <numeric>.
    Code
      check_r6(1, .envir = e)
    Condition
      Error:
      ! `1` must inherit from <R6>, but is class <numeric>.

