#' Object property checks
#'
#' @description
#' Check if inputs have certain properties and error if not.
#' @param x An **R** object.
#' @param n,nrow,ncol The expected length/size, number of columns, or number of
#' rows of `x`.
#' @param ... Additional arguments passed to [cli_abort()][cli::cli_abort]
#' which forwards unmatched arguments to [abort()][rlang::abort].
#' @param allow_null Whether `x` is allowed to be `NULL`.
#' @param unique Whether `x` is required to have unique names.
#' @param allow_empty Whether `x` is allowed to have empty names (`""`).
#' @inheritParams rlang::args_error_context
#' @return `NULL` invisibly if the check passes, otherwise an error is thrown.
#' @details
#' Input types are not checked to be of expected types, they are passed
#' 'as is' to the functions that do the property checking. The only exception
#' is for `NULL` inputs, which error if `allow_null = FALSE`.
#'
#' `check_size()` uses [`vec_size()`][vctrs::vec_size] to determine the size
#' of `x`, as opposed to [`length()`][base::length].
#' @name property-checks
#' @family checks
#' @examples
#' x <- c(1, 2, NA)
#' check_length(x, 4) |> try()
#' check_size(x, 4) |> try()
#'
#' # length modifiers can be used
#' check_length(x, at_most(2)) |> try()
#'
#' x <- data.frame(x = 1)
#' check_nrow(x, 2) |> try()
#' check_ncol(x, in_range(2, 4)) |> try()
#' check_size(x, at_least(2)) |> try()
#'
#' x <- numeric(0)
#' check_non_empty(x) |> try()
#' check_non_empty(NULL) |> try()
#'
#' x <- c(1, 2, 3)
#' check_named(x) |> try()
#' names(x) <- c("a", "b", "a")
#' check_named(x, unique = TRUE) |> try()
#' names(x) <- c("a", "b", "")
#' check_named(x, allow_empty = FALSE) |> try()
#'
#' check_length(NULL, 2, allow_null = TRUE)
NULL

#' @rdname property-checks
#' @export
check_length <- function(
  x,
  n,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if ((i <- is.null(x)) && allow_null) {
    return(invisible(NULL))
  } else if (i && !allow_null) {
    cli_abort(
      message = format_inline("{.arg {arg}} must not be {.cls NULL}."),
      ...,
      call = call
    )
  }

  do_n_check(x, n, NULL, ..., arg = arg, call = call)
}

#' @rdname property-checks
#' @export
check_nrow <- function(
  x,
  nrow,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if ((i <- is.null(x)) && allow_null) {
    return(invisible(NULL))
  } else if (i && !allow_null) {
    cli_abort(
      message = format_inline("{.arg {arg}} must not be {.cls NULL}."),
      ...,
      call = call
    )
  }

  do_nrow_check(x, nrow, NULL, ..., arg = arg, call = call)
}

#' @rdname property-checks
#' @export
check_ncol <- function(
  x,
  ncol,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if ((i <- is.null(x)) && allow_null) {
    return(invisible(NULL))
  } else if (i && !allow_null) {
    cli_abort(
      message = format_inline("{.arg {arg}} must not be {.cls NULL}."),
      ...,
      call = call
    )
  }

  do_ncol_check(x, ncol, NULL, ..., arg = arg, call = call)
}

#' @rdname property-checks
#' @export
check_size <- function(
  x,
  n,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if ((i <- is.null(x)) && allow_null) {
    return(invisible(NULL))
  } else if (i && !allow_null) {
    cli_abort(
      message = format_inline("{.arg {arg}} must not be {.cls NULL}."),
      ...,
      call = call
    )
  }

  do_size_check(x, n, NULL, ..., arg = arg, call = call)
}

#' @rdname property-checks
#' @export
check_non_empty <- function(
  x,
  ...,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!length(x)) {
    cli_abort(
      message = format_inline("{.arg {arg}} must not be empty."),
      ...,
      call = call
    )
  }

  invisible(NULL)
}

#' @rdname property-checks
#' @export
check_named <- function(
  x,
  ...,
  unique = FALSE,
  allow_empty = TRUE,
  arg = caller_arg(x),
  call = caller_env()
) {
  nms <- names(x)

  if (is.null(nms)) {
    cli_abort(
      message = format_inline("{.arg {arg}} must be named."),
      ...,
      call = call
    )
  }

  if (unique && anyDuplicated(nms)) {
    dups <- nms[duplicated(nms)]

    cli_abort(
      message = format_inline(
        "{.arg {arg}} must have unique names. Duplicates: {.val {dups}}."
      ),
      ...,
      call = call
    )
  }

  if (!allow_empty && any(!nzchar(nms))) {
    cli_abort(
      message = format_inline("{.arg {arg}} must not contain empty names."),
      ...,
      call = call
    )
  }

  invisible(NULL)
}
