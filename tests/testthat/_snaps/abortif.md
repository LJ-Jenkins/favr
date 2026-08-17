# abortif error message indicates issue

    Code
      abortif(1)
    Condition
      Error:
      ! `1` must be a <logical> vector, not the number 1.
    Code
      abortif(1:5)
    Condition
      Error:
      ! `1:5` must be a <logical> vector, not an <integer> vector.
    Code
      abortif(c(FALSE, NA))
    Condition
      Error:
      ! `c(FALSE, NA)` must not contain NA values.
    Code
      abortif(NA)
    Condition
      Error:
      ! `NA` must not be NA.
    Code
      abortif(TRUE)
    Condition
      Error:
      ! `TRUE` is TRUE.

# abortif error message includes custom default message

    Code
      x <- 1:3
      abortif(!is.character(x), message = "{.pkg cli} formatted info: {.val {x}}")
    Condition
      Error:
      ! cli formatted info: 1, 2, and 3

# abortif error mesage derived from names

    Code
      x <- 1:3
      abortif(`{.pkg cli} message for {.val {x}}` = !is.character(x))
    Condition
      Error:
      ! cli message for 1, 2, and 3

# abortif shows error about abort_args if error occurring

    Code
      abortifnot(1, abort_args = "not a list")
    Condition
      Error:
      ! `abort_args` must be a <list>, not the string "not a list".

# abortif passes abort_args

    Code
      x <- 1:3
      abortif(!is.character(x), abort_args = list(footer = "custom footer"))
    Condition
      Error:
      ! `!is.character(x)` is TRUE.
      custom footer

# abortif default errors display properly no matter the .envir

    Code
      e <- new.env()
      e$arg <- "Shouldn't show"
      abortif(1, .envir = e)
    Condition
      Error:
      ! `1` must be a <logical> vector, not the number 1.

# abortif custom name/message errors use .envir

    Code
      e <- new.env()
      e$arg <- "Should show"
      abortif(`custom message: {.arg {arg}}` = 1, .envir = e)
    Condition
      Error:
      ! custom message: `Should show`
    Code
      abortif(1, message = "custom message: {.arg {arg}}", .envir = e)
    Condition
      Error:
      ! custom message: `Should show`

