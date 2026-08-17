# check error message includes custom default message

    Code
      x <- 1:3
      check(is.character(x), message = "{.pkg cli} formatted info: {.val {x}}")
    Condition
      Error:
      ! cli formatted info: 1, 2, and 3

# check error mesage derived from names

    Code
      x <- 1:3
      check(`{.pkg cli} message for {.val {x}}` = is.character(x))
    Condition
      Error:
      ! cli message for 1, 2, and 3

# check shows error about abort_args if error occurring

    Code
      check(1, abort_args = "not a list")
    Condition
      Error:
      ! `abort_args` must be a <list>, not the string "not a list".

# check passes abort_args

    Code
      x <- 1:3
      check(is.character(x), abort_args = list(footer = "custom footer"))
    Condition
      Error:
      ! `is.character(x)` is not TRUE.
      custom footer

# check default errors display properly no matter the .envir

    Code
      e <- new.env()
      e$arg <- "Shouldn't show"
      check(1, .envir = e)
    Condition
      Error:
      ! `1` must be a <logical> vector, not the number 1.

# check custom name/message errors use .envir

    Code
      e <- new.env()
      e$arg <- "Should show"
      check(`custom message: {.arg {arg}}` = 1, .envir = e)
    Condition
      Error:
      ! custom message: `Should show`
    Code
      check(1, message = "custom message: {.arg {arg}}", .envir = e)
    Condition
      Error:
      ! custom message: `Should show`

