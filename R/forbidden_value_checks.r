#' Forbidden value checks
#'
#' @description
#' Check if inputs contain forbidden values and error if so.
#' @param x An object to check.
#' @param ... Additional arguments passed to [`cli_abort()`][cli::cli_abort]
#' which forwards unmatched arguments to [`abort()`][rlang::abort].
#' @param allow_null Whether `x` is allowed to be `NULL`.
#' @param allow_all_ws Whether `x` is allowed to contain elements that are
#' all whitespace.
#' @inheritParams rlang::args_error_context
#' @return `NULL` invisibly if the check passes, otherwise an error is thrown.
#' @details
#' `NA` checks are done with [`anyNA()`];
#'
#' finite checks are done with [`any()`] and [`is.finite()`];
#'
#' unique checks are done with [`anyDuplicated()`];
#'
#' zero chr checks are done with [`any()`] and [`nzchar()`];
#'
#' If `allow_all_ws = FALSE` then whitespace elements are identified using
#' [`grepl("\\s+", x)`][grepl].
#'
#' Input types are not checked, they are passed 'as is' to the functions
#' that do the forbidden value checking. The only exception is for `NULL`
#' inputs, which error if `allow_null = FALSE`.
#' @note
#' `NA_character_` is not considered zero chr nor all whitespace.
#' @name forbidden-value-checks
#' @family checks
#' @examples
#' x <- c(1, 2, NA)
#' check_no_na(x) |> try()
#'
#' x <- c(1, 2, Inf)
#' check_finite(x) |> try()
#'
#' x <- c(1, 2, 3, 1)
#' check_unique(x) |> try()
#'
#' x <- c("a", "b", "")
#' check_nzchar(x) |> try()
#'
#' x <- c("a", "b", " ")
#' check_nzchar(x, allow_all_ws = FALSE) |> try()
NULL

#' @rdname forbidden-value-checks
#' @export
check_no_na <- function(
  x,
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

  na_check(FALSE, x, length(x), arg, ..., call = call)
}

#' @rdname forbidden-value-checks
#' @export
check_finite <- function(
  x,
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

  finite_check(TRUE, x, length(x), arg, ..., call = call)
}

#' @rdname forbidden-value-checks
#' @export
check_unique <- function(
  x,
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

  if (anyDuplicated(x)) {
    dups <- x[duplicated(x)]

    cli_abort(
      message = format_inline(
        "{.arg {arg}} must have unique elements. Duplicates: {.val {dups}}."
      ),
      ...,
      call = call
    )
  }

  invisible(NULL)
}

#' @rdname forbidden-value-checks
#' @export
check_nzchar <- function(
  x,
  ...,
  allow_all_ws = TRUE,
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

  if (length(x) == 1L) {
    empty_string_check(FALSE, x, arg, ..., call = call)
  } else {
    nzchar_check(FALSE, x, arg, ..., call = call)
  }

  all_ws_check(allow_all_ws, x, arg, ..., call = call)
}

na_check <- function(allow_na, x, n, arg, ..., call = caller_env()) {
  if (!allow_na && anyNA(x)) {
    cli_abort(
      message = na_msg(arg, n, x),
      ...,
      call = call
    )
  }

  invisible(NULL)
}

finite_check <- function(finite, x, n, arg, ..., call = caller_env()) {
  # maybe switch to suppr::anyNF in future
  # or just implement anyNF here
  if (finite && any(!is.finite(x))) {
    cli_abort(
      message = non_finite_msg(arg, n, x),
      ...,
      call = call
    )
  }

  invisible(NULL)
}

nzchar_check <- function(allow_nzchar, x, arg, ..., call = caller_env()) {
  # maybe switch to suppr::anyZchar in future
  if (!allow_nzchar && any(!nzchar(x))) {
    cli_abort(
      message = format_inline("{.arg {arg}} must not contain empty strings."),
      ...,
      call = call
    )
  }

  invisible(NULL)
}

all_ws_check <- function(allow_all_ws, x, arg, ..., call = caller_env()) {
  # maybe switch to suppr::anyWS in future
  if (!allow_all_ws && any(grepl("\\s+", x))) {
    msg <- if (length(x) == 1L) {
      "{.arg {arg}} must not be all whitespace."
    } else {
      "{.arg {arg}} must not contain all whitespace elements."
    }

    cli_abort(
      message = format_inline(msg),
      ...,
      call = call
    )
  }

  invisible(NULL)
}

empty_string_check <- function(allow_empty, x, arg, ..., call = caller_env()) {
  if (!allow_empty && !nzchar(x)) {
    # eagerly eval message in case odd `.envir` given
    cli_abort(
      message = format_inline("{.arg {arg}} must not be an empty string."),
      ...,
      call = call
    )
  }

  invisible(NULL)
}
