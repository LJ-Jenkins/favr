#-- n

n_check <- function(x, n, expected_type, ..., arg, call) {
  if (!is.null(n)) {
    do_n_check(x, n, expected_type, ..., arg = arg, call = call)
  }

  invisible(NULL)
}

do_n_check <- function(x, n, ...) {
  UseMethod("do_n_check", n)
}

#' @export
do_n_check.default <- function(x, n, expected_type, ..., arg, call) {
  n <- vec_cast(n, integer(), x_arg = "n", call = caller_env())
  check_n(n, n_arg = "n", call = caller_env())

  if (length(x) != n) {
    cli_abort(
      message = wrong_length_msg(arg, expected_type, n, x),
      ...,
      call = call
    )
  }
}

#' @export
do_n_check.favr_at_least <- function(x, n, expected_type, ..., arg, call) {
  if (length(x) < n[["at_least"]]) {
    cli_abort(
      message = at_least_msg(arg, expected_type, n, x),
      ...,
      call = call
    )
  }
}

#' @export
do_n_check.favr_at_most <- function(x, n, expected_type, ..., arg, call) {
  if (length(x) > n[["at_most"]]) {
    cli_abort(
      message = at_most_msg(arg, expected_type, n, x),
      ...,
      call = call
    )
  }
}

#' @export
do_n_check.favr_in_range <- function(x, n, expected_type, ..., arg, call) {
  x_n <- length(x)
  if (x_n < n[["at_least"]] || x_n > n[["at_most"]]) {
    cli_abort(
      message = in_range_msg(arg, expected_type, n, x),
      ...,
      call = call
    )
  }
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

#-- nrow

nrow_check <- function(x, nrow, expected_type, ..., arg, call) {
  if (!is.null(nrow)) {
    do_nrow_check(x, nrow, expected_type, ..., arg = arg, call = call)
  }

  invisible(NULL)
}

do_nrow_check <- function(x, nrow, ...) {
  UseMethod("do_nrow_check", nrow)
}

#' @export
do_nrow_check.default <- function(x, nrow, expected_type, ..., arg, call) {
  nrow <- vec_cast(nrow, integer(), x_arg = "nrow", call = caller_env())
  check_n(nrow, n_arg = "nrow", call = caller_env())

  x_nrow <- nrow(x)
  if (is.null(x_nrow)) {
    cli_abort(
      "{.arg {arg}} has no row dimension to check.",
      ...,
      call = call
    )
  }

  if (x_nrow != nrow) {
    cli_abort(
      message = wrong_nrow_ncol_msg(arg, expected_type, nrow, x_nrow),
      ...,
      call = call
    )
  }
}

#' @export
do_nrow_check.favr_at_least <- function(
  x,
  nrow,
  expected_type,
  ...,
  arg,
  call
) {
  x_nrow <- nrow(x)
  if (is.null(x_nrow)) {
    cli_abort(
      "{.arg {arg}} has no row dimension to check.",
      ...,
      call = call
    )
  }

  if (x_nrow < nrow[["at_least"]]) {
    cli_abort(
      message = wrong_nrow_ncol_msg(
        arg,
        expected_type,
        nrow[["at_least"]],
        x_nrow,
        modifier = "at_least"
      ),
      ...,
      call = call
    )
  }
}

#' @export
do_nrow_check.favr_at_most <- function(
  x,
  nrow,
  expected_type,
  ...,
  arg,
  call
) {
  x_nrow <- nrow(x)
  if (is.null(x_nrow)) {
    cli_abort(
      "{.arg {arg}} has no row dimension to check.",
      ...,
      call = call
    )
  }

  if (x_nrow > nrow[["at_most"]]) {
    cli_abort(
      message = wrong_nrow_ncol_msg(
        arg,
        expected_type,
        nrow[["at_most"]],
        x_nrow,
        modifier = "at_most"
      ),
      ...,
      call = call
    )
  }
}

#' @export
do_nrow_check.favr_in_range <- function(
  x,
  nrow,
  expected_type,
  ...,
  arg,
  call
) {
  x_nrow <- nrow(x)
  if (is.null(x_nrow)) {
    cli_abort(
      "{.arg {arg}} has no row dimension to check.",
      ...,
      call = call
    )
  }

  if (x_nrow < nrow[["at_least"]] || x_nrow > nrow[["at_most"]]) {
    cli_abort(
      message = wrong_nrow_ncol_msg(
        arg,
        expected_type,
        nrow[c("at_least", "at_most")],
        x_nrow,
        modifier = "in_range"
      ),
      ...,
      call = call
    )
  }
}

#-- ncol

ncol_check <- function(x, ncol, expected_type, ..., arg, call) {
  if (!is.null(ncol)) {
    do_ncol_check(x, ncol, expected_type, ..., arg = arg, call = call)
  }

  invisible(NULL)
}

do_ncol_check <- function(x, ncol, ...) {
  UseMethod("do_ncol_check", ncol)
}

#' @export
do_ncol_check.default <- function(x, ncol, expected_type, ..., arg, call) {
  ncol <- vec_cast(ncol, integer(), x_arg = "ncol", call = caller_env())
  check_n(ncol, n_arg = "ncol", call = caller_env())

  x_ncol <- ncol(x)
  if (is.null(x_ncol)) {
    cli_abort(
      "{.arg {arg}} has no column dimension to check.",
      ...,
      call = call
    )
  }

  if (x_ncol != ncol) {
    cli_abort(
      message = wrong_nrow_ncol_msg(arg, expected_type, ncol, x_ncol),
      ...,
      call = call
    )
  }
}

#' @export
do_ncol_check.favr_at_least <- function(
  x,
  ncol,
  expected_type,
  ...,
  arg,
  call
) {
  x_ncol <- ncol(x)
  if (is.null(x_ncol)) {
    cli_abort(
      "{.arg {arg}} has no column dimension to check.",
      ...,
      call = call
    )
  }

  if (x_ncol < ncol[["at_least"]]) {
    cli_abort(
      message = wrong_nrow_ncol_msg(
        arg,
        expected_type,
        ncol[["at_least"]],
        x_ncol,
        modifier = "at_least",
        case = "column"
      ),
      ...,
      call = call
    )
  }
}

#' @export
do_ncol_check.favr_at_most <- function(
  x,
  ncol,
  expected_type,
  ...,
  arg,
  call
) {
  x_ncol <- ncol(x)
  if (is.null(x_ncol)) {
    cli_abort(
      "{.arg {arg}} has no column dimension to check.",
      ...,
      call = call
    )
  }

  if (x_ncol > ncol[["at_most"]]) {
    cli_abort(
      message = wrong_nrow_ncol_msg(
        arg,
        expected_type,
        ncol[["at_most"]],
        x_ncol,
        modifier = "at_most",
        case = "column"
      ),
      ...,
      call = call
    )
  }
}

#' @export
do_ncol_check.favr_in_range <- function(
  x,
  ncol,
  expected_type,
  ...,
  arg,
  call
) {
  x_ncol <- ncol(x)
  if (is.null(x_ncol)) {
    cli_abort(
      "{.arg {arg}} has no column dimension to check.",
      ...,
      call = call
    )
  }

  if (x_ncol < ncol[["at_least"]] || x_ncol > ncol[["at_most"]]) {
    cli_abort(
      message = wrong_nrow_ncol_msg(
        arg,
        expected_type,
        ncol[c("at_least", "at_most")],
        x_ncol,
        modifier = "in_range",
        case = "column"
      ),
      ...,
      call = call
    )
  }
}

wrong_nrow_ncol_msg <- function(
  arg,
  expected_type,
  expected_n,
  given,
  modifier = "null",
  case = "row"
) {
  cases <- paste0(case, "s")

  switch(modifier,
    at_least = format_inline(
      "{.arg {arg}} must be {expected_type}",
      " with at least {.val {expected_n}} ",
      "{if (expected_n == 1) case else cases}",
      ", but it has {.val {given}}."
    ),
    at_most = format_inline(
      "{.arg {arg}} must be {expected_type}",
      " with at most {.val {expected_n}} ",
      "{if (expected_n == 1) case else cases}",
      ", but it has {.val {given}}."
    ),
    in_range = format_inline(
      "{.arg {arg}} must be {expected_type}",
      " with {.val {expected_n[1]}} to {.val {expected_n[2]}} {cases}",
      ", but it has {.val {given}}."
    ),
    format_inline(
      "{.arg {arg}} must be {expected_type}",
      " with {.val {expected_n}} ",
      "{if (expected_n == 1) case else cases}",
      ", not {.val {given}}."
    )
  )
}
