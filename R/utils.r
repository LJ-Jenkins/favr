do_abort <- function(message, dots, call) {
  do.call(cli_abort, c(list(message = message, call = call), dots))
}

is_one <- function(n) {
  if (!is.null(n) && n == 1L) {
    TRUE
  } else {
    FALSE
  }
}

extract_braces <- function(x) {
  sub(".*(\\{[^}]*\\}).*", "\\1", x)
}

# if NULL or "" return, else rhs
`%&&""%` <- function(lhs, rhs) {
  if (is.null(lhs) || !nzchar(lhs)) {
    lhs
  } else {
    rhs
  }
}

# previously had all messages with paste0() to be formatted within
# cli_abort(), but this meant a user passed `.envir` had to be
# handled everywhere and every doc had to have a note saying
# `.envir` silently ignored (even if it wouldn't be useful for a
# user to pass it in).

wrong_type_msg <- function(
  arg,
  expected_type,
  given,
  value = TRUE,
  length = FALSE
) {
  format_inline(
    "{.arg {arg}} must be {expected_type}, not ",
    type_friendly(given, value = value, length = length), "."
  )
}

wrong_length_msg <- function(
  arg,
  expected_type,
  expected_length,
  given
) {
  format_inline(
    "{.arg {arg}} must be {expected_type}",
    " of length {.val {expected_length}}",
    ", not {.val {length(given)}}."
  )
}

wrong_scalar_length_msg <- function(
  arg,
  expected_type,
  given
) {
  format_inline(
    "{.arg {arg}} must be {expected_type}",
    ", but it is of length {.val {length(given)}}."
  )
}

at_least_msg <- function(
  arg,
  expected_type,
  expected_length,
  given
) {
  format_inline(
    "{.arg {arg}} must be {expected_type}",
    " of at least length {.val {expected_length}}",
    ", but it is of length {.val {length(given)}}."
  )
}

at_most_msg <- function(
  arg,
  expected_type,
  expected_length,
  given
) {
  format_inline(
    "{.arg {arg}} must be {expected_type}",
    " of at most length {.val {expected_length}}",
    ", but it is of length {.val {length(given)}}."
  )
}

in_range_msg <- function(
  arg,
  expected_type,
  expected_length,
  given
) {
  format_inline(
    "{.arg {arg}} must be {expected_type} of a length between ",
    "{.val {expected_length}}, but it is of length ",
    "{.val {length(given)}}."
  )
}

na_msg <- function(arg, n) {
  if (is_one(n)) {
    format_inline("{.arg {arg}} must not be {.val {NA}}.")
  } else {
    format_inline("{.arg {arg}} must not contain {.val {NA}} values.")
  }
}

non_finite_msg <- function(arg, n, x) {
  if (is_one(n)) {
    format_inline("{.arg {arg}} must be a finite value, not {.val {x}}.")
  } else {
    format_inline("{.arg {arg}} must not contain non-finite values.")
  }
}
