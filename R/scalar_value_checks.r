#' Scalar value checks
#'
#' @description
#' Check if inputs are expected scalar values and
#' throw an error if not.
#' @param x An object to check.
#' @param ... Additional arguments passed to [cli_abort()][cli::cli_abort]
#' which forwards unmatched arguments to [abort()][rlang::abort].
#' @param allow_null Whether `x` is allowed to be `NULL`.
#' @inheritParams rlang::args_error_context
#' @param string A character vector of allowed values for `x`. If `NULL`,
#' the value is not checked. The check passes if `x` is **any** of the
#' values in `string`.
#' @param allow_empty Whether `x` is allowed to be an empty string
#' (i.e. when `FALSE` `""` is not allowed).
#' @return `NULL` invisibly if the check passes, otherwise an error is thrown.
#' @name scalar-value-checks
#' @family checks
#' @examples
#' x <- TRUE
#' check_true(x)
#' check_false(x) |> try()
#'
#' check_bool(NA) |> try()
#' check_bool(NULL, allow_null = TRUE)
#'
#' x <- "a"
#' check_string(x)
#' check_string(x, string = c("a", "b"))
#' check_string(x, string = c("b", "c")) |> try()
#' check_string("", allow_empty = FALSE) |> try()
NULL

#' @rdname scalar-value-checks
#' @export
check_true <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (allow_null && is.null(x)) {
    return(invisible(NULL))
  }

  if (!isTRUE(x)) {
    do_abort(
      message = wrong_type_msg(
        arg,
        "a single {.val {TRUE}}",
        x,
        length = if (is.logical(x)) TRUE else FALSE
      ),
      dots = list(...),
      call = call
    )
  }

  invisible(NULL)
}

#' @rdname scalar-value-checks
#' @export
check_false <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (allow_null && is.null(x)) {
    return(invisible(NULL))
  }

  if (!isFALSE(x)) {
    do_abort(
      message = wrong_type_msg(
        arg,
        "a single {.val {FALSE}}",
        x,
        length = if (is.logical(x)) TRUE else FALSE
      ),
      dots = list(...),
      call = call
    )
  }

  invisible(NULL)
}

#' @rdname scalar-value-checks
#' @export
check_bool <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (allow_null && is.null(x)) {
    return(invisible(NULL))
  }

  if (!is_bool(x)) {
    do_abort(
      message = wrong_type_msg(
        arg,
        "a single {.val {TRUE}} or {.val {FALSE}}",
        x,
        length = if (is.logical(x)) TRUE else FALSE
      ),
      dots = list(...),
      call = call
    )
  }

  invisible(NULL)
}

#' @rdname scalar-value-checks
#' @export
check_string <- function(
  x,
  ...,
  string = NULL,
  allow_empty = TRUE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (allow_null && is.null(x)) {
    return(invisible(NULL))
  }

  dots <- list(...)

  if ((i <- !is.null(string)) && !is.character(string)) {
    do_abort(
      message = wrong_type_msg(
        "string",
        "a {.cls character} vector",
        string
      ),
      dots = dots,
      call = call
    )
  }

  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    do_abort(
      message = wrong_type_msg(
        arg,
        "a single string",
        x,
        length = if (is.character(x)) TRUE else FALSE
      ),
      dots = dots,
      call = call
    )
  }

  if (!allow_empty && !nzchar(x)) {
    # eagerly eval message in case odd `.envir` given
    do_abort(
      message = format_inline("{.arg {arg}} must not be an empty string."),
      dots = dots,
      call = call
    )
  }

  if (i && !x %in% string) {
    do_abort(
      message = format_inline("{.arg {arg}} must be one of {.or {.val {string}}}."),
      dots = dots,
      call = call
    )
  }

  invisible(NULL)
}
