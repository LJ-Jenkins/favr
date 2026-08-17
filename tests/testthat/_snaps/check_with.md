# check_with error message includes custom default message

    Code
      x <- "shouldn't show"
      check_with(list(x = 1:3), is.character(x), message = "{.pkg cli} formatted info: {.val {x}}")
    Condition
      Error:
      ! cli formatted info: 1, 2, and 3

# check_with error messages eval in .data then .envir

    Code
      x <- "a"
      y <- list(x = 1:3)
      check_with(y, `{.pkg cli} message for {.val {x}}` = is.character(x))
    Condition
      Error:
      ! cli message for 1, 2, and 3

---

    Code
      b <- "+"
      y <- list(x = 1)
      check_with(y, `{b}` = is.character(x))
    Condition
      Error:
      ! +

---

    Code
      b <- "+"
      e <- new.env()
      e$b <- "$"
      y <- list(x = 1)
      check_with(y, `{b}` = is.character(x), .envir = e)
    Condition
      Error:
      ! $

# check_with error messages support pronouns

    Code
      x <- "a"
      y <- list(x = 1:3)
      check_with(y, `{ .data$x}` = is.character(x))
    Condition
      Error:
      ! 1, 2, and 3

---

    Code
      x <- "+"
      y <- list(x = 1)
      check_with(y, `{ .env$x}` = is.character(x))
    Condition
      Error:
      ! +

# check_with shows error about abort_args if error occurring

    Code
      check_with(list(x = 1), x, abort_args = "not a list")
    Condition
      Error:
      ! `abort_args` must be a <list>, not the string "not a list".

# check_with passes abort_args

    Code
      x <- 1:3
      check_with(list(x = x), is.character(x), abort_args = list(footer = "custom footer"))
    Condition
      Error:
      ! `is.character(x)` is not TRUE.
      custom footer

# check_with default errors display properly no matter the .envir

    Code
      e <- new.env()
      e$arg <- "Shouldn't show"
      check_with(list(x = 1), x, .envir = e)
    Condition
      Error:
      ! `x` must be a <logical> vector, not the number 1.

# check_with custom name/message errors use .envir

    Code
      e <- new.env()
      e$arg <- "Should show"
      check_with(`custom message: {.arg {arg}}` = 1, .envir = e)
    Condition
      Error in `check_with()`:
      ! argument ".data" is missing, with no default
    Code
      check_with(list(x = 1), x, message = "custom message: {.arg {arg}}", .envir = e)
    Condition
      Error:
      ! custom message: `Should show`

# check_with works with `{{` defusing

    Code
      df <- data.frame(x = 1, y = 2)
      f <- (function(data, var1, var2) {
        check_with(data, {{ var1 }} < {{ var2 }})
      })
      f(df, y, x)
    Condition
      Error in `f()`:
      ! `y < x` is not TRUE.

