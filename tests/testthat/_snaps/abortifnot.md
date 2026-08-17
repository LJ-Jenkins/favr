# abortifnot error message indicates issue

    Code
      abortifnot(1)
    Condition
      Error:
      ! `1` must be a <logical> vector, not the number 1.
    Code
      abortifnot(1:5)
    Condition
      Error:
      ! `1:5` must be a <logical> vector, not an <integer> vector.
    Code
      abortifnot(c(TRUE, NA))
    Condition
      Error:
      ! `c(TRUE, NA)` must not contain NA values.
    Code
      abortifnot(NA)
    Condition
      Error:
      ! `NA` must not be NA.
    Code
      abortifnot(FALSE)
    Condition
      Error:
      ! `FALSE` is not TRUE.

# abortifnot error message includes custom default message

    Code
      x <- 1:3
      abortifnot(is.character(x), message = "{.pkg cli} formatted info: {.val {x}}")
    Condition
      Error:
      ! cli formatted info: 1, 2, and 3

# abortifnot error mesage derived from names

    Code
      x <- 1:3
      abortifnot(`{.pkg cli} message for {.val {x}}` = is.character(x))
    Condition
      Error:
      ! cli message for 1, 2, and 3

# abortifnot shows error about abort_args if error occurring

    Code
      abortifnot(1, abort_args = "not a list")
    Condition
      Error:
      ! `abort_args` must be a <list>, not the string "not a list".

# abortifnot passes abort_args

    Code
      x <- 1:3
      abortifnot(is.character(x), abort_args = list(footer = "custom footer"))
    Condition
      Error:
      ! `is.character(x)` is not TRUE.
      custom footer

# abortifnot default errors display properly no matter the .envir

    Code
      e <- new.env()
      e$arg <- "Shouldn't show"
      abortifnot(1, .envir = e)
    Condition
      Error:
      ! `1` must be a <logical> vector, not the number 1.

# abortifnot custom name/message errors use .envir

    Code
      e <- new.env()
      e$arg <- "Should show"
      abortifnot(`custom message: {.arg {arg}}` = 1, .envir = e)
    Condition
      Error:
      ! custom message: `Should show`
    Code
      abortifnot(1, message = "custom message: {.arg {arg}}", .envir = e)
    Condition
      Error:
      ! custom message: `Should show`

