#-- vec_size

# not needed atm
# size_check <- function(x, n, expected_type, ..., arg, call) {
#   if (!is.null(n)) {
#     do_size_check(x, n, expected_type, ..., arg = arg, call = call)
#   }

#   invisible(NULL)
# }

do_size_check <- function(x, n, ...) {
  UseMethod("do_size_check", n)
}

#' @export
do_size_check.default <- function(x, n, expected_type, ..., arg, call) {
  n <- vec_cast(n, integer(), x_arg = "n", call = caller_env())
  check_n(n, n_arg = "n", call = caller_env())

  if (vec_size(x) != n) {
    cli_abort(
      message = wrong_size_msg(arg, expected_type, n, x),
      ...,
      call = call
    )
  }
}

#' @export
do_size_check.favr_at_least <- function(x, n, expected_type, ..., arg, call) {
  if (vec_size(x) < n[["at_least"]]) {
    cli_abort(
      message = size_at_least_msg(arg, expected_type, n, x),
      ...,
      call = call
    )
  }
}

#' @export
do_size_check.favr_at_most <- function(x, n, expected_type, ..., arg, call) {
  if (vec_size(x) > n[["at_most"]]) {
    cli_abort(
      message = size_at_most_msg(arg, expected_type, n, x),
      ...,
      call = call
    )
  }
}

#' @export
do_size_check.favr_in_range <- function(x, n, expected_type, ..., arg, call) {
  x_n <- vec_size(x)
  if (x_n < n[["at_least"]] || x_n > n[["at_most"]]) {
    cli_abort(
      message = size_in_range_msg(arg, expected_type, n, x),
      ...,
      call = call
    )
  }
}

wrong_size_msg <- function(
  arg,
  expected_type,
  expected_size,
  given
) {
  if (!is.null(expected_type)) expected_type <- paste0(" ", expected_type)

  format_inline(
    "{.arg {arg}} must be{expected_type}",
    " of size {.val {expected_size}}",
    ", not {.val {vec_size(given)}}."
  )
}

size_at_least_msg <- function(
  arg,
  expected_type,
  expected_size,
  given
) {
  if (!is.null(expected_type)) expected_type <- paste0(" ", expected_type)

  format_inline(
    "{.arg {arg}} must be{expected_type}",
    " of at least size {.val {expected_size}}",
    ", but it is of size {.val {vec_size(given)}}."
  )
}

size_at_most_msg <- function(
  arg,
  expected_type,
  expected_size,
  given
) {
  if (!is.null(expected_type)) expected_type <- paste0(" ", expected_type)

  format_inline(
    "{.arg {arg}} must be{expected_type}",
    " of at most size {.val {expected_size}}",
    ", but it is of size {.val {vec_size(given)}}."
  )
}

size_in_range_msg <- function(
  arg,
  expected_type,
  expected_size,
  given
) {
  if (!is.null(expected_type)) expected_type <- paste0(" ", expected_type)

  format_inline(
    "{.arg {arg}} must be{expected_type} of a size between ",
    "{.val {expected_size}}, but it is of size ",
    "{.val {vec_size(given)}}."
  )
}
