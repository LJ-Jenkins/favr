#' S3 check builders
#'
#' @description
#' Check builders for S3 types. These functions can be used to create
#' custom S3 type checks in the style of favr.
#' @param x An object to check.
#' @param n,nrow,ncol The expected length, number of columns, or number of
#' rows of `x`.
#' @param type The expected S3 type of `x`.
#' @param type_msg A message describing the expected S3 type of
#' `x` for use in error messages not relating to inheritance, optionally
#' with [cli] formatting. See details.
#' @param ... Additional arguments passed to [cli_abort()][cli::cli_abort]
#' which forwards unmatched arguments to [abort()][rlang::abort].
#' @param allow_null Whether `x` is allowed to be `NULL`.
#' @inheritParams rlang::args_error_context
#' @return `NULL` invisibly if the check passes, otherwise an error is thrown.
#' @details
#' Inputs are passed to [check_inherits()] to check that `x` inherits from the
#' expected S3 type. This means that error messages about inheritance will
#' always show the expected S3 type in the cli format of
#' `{.cls <expected_s3_type>}`.
#'
#' The `type_msg` argument is used to customise the error message when a
#' different check fails (e.g., length), where the grammar may require
#' different phrasing. For example, the default value is
#' `"a {.cls <expected_s3_type>}"`, but many favr functions use
#' `"a {.cls <expected_s3_type>} vector"`. Also consider where 'an' is
#' more appropriate than 'a'.
#'
#' These functions can be used with the [bare()] modifier to check if an object
#' is a bare S3 object (where the expected S3 type is the first class in the
#' class attribute of `x`), and the length modifiers [at_least()], [at_most()],
#' and [in_range()] to modify the behaviour of the length checking `n`,
#' `nrow`, and `ncol` arguments.
#' @note
#' Although named `_vec` and `_df`, these functions could be used to check any
#' S3 type, not just vectors and data frames. Their names are intended to
#' indicate the expected behaviour of the check - for types that would
#' use the length or dimension checking arguments.
#' @name s3-check-builders
#' @family check-builders
#' @examples
#' # Create a custom type check for a hypothetical "my_class" S3 class
#' check_my_class <- function(
#'   x,
#'   n = NULL,
#'   ...,
#'   allow_null = FALSE,
#'   arg = caller_arg(x),
#'   call = caller_env()
#' ) {
#'   s3_vec_check(
#'     x,
#'     n,
#'     type = "my_class",
#'     type_msg = "a {.cls my_class} vector",
#'     ...,
#'     allow_null = allow_null,
#'     arg = arg,
#'     call = call
#'   )
#' }
#'
#' # inheritance errors use 'type'
#' check_my_class(1L) |> try()
#'
#' x <- structure(1:3, class = "my_class")
#' check_my_class(x)
#' check_my_class(NULL, allow_null = TRUE)
#'
#' # other errors use 'type_msg'
#' check_my_class(x, n = 2) |> try()
#' check_my_class(x, n = at_least(4)) |> try()
#' check_my_class(x, n = at_most(2)) |> try()
#' check_my_class(x, n = in_range(1, 2)) |> try()
#'
#' class(x) <- c("another_class", class(x))
#' check_my_class(bare(x)) |> try()
NULL

#' @rdname s3-check-builders
#' @export
s3_vec_check <- function(
  x,
  n,
  type,
  type_msg = paste0("a {.cls ", type, "}"),
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  x_modifier <- inherits(x, "favr_modifier")
  if (x_modifier) {
    arg <- x[["arg"]]
    x <- x[["obj"]]
  }

  if (allow_null && is.null(x)) {
    return(invisible(NULL))
  }

  do_s3_check(
    x,
    x_modifier,
    type,
    type_msg,
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )

  n_check(x, n, type_msg, ..., arg = arg, call = call)
}

#' @rdname s3-check-builders
#' @export
s3_df_check <- function(
  x,
  nrow,
  ncol,
  type,
  type_msg = paste0("a {.cls ", type, "}"),
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  x_modifier <- inherits(x, "favr_modifier")
  if (x_modifier) {
    arg <- x[["arg"]]
    x <- x[["obj"]]
  }

  if (allow_null && is.null(x)) {
    return(invisible(NULL))
  }

  do_s3_check(
    x,
    x_modifier,
    type,
    type_msg,
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )

  nrow_check(x, nrow, type_msg, ..., arg = arg, call = call)

  ncol_check(x, ncol, type_msg, ..., arg = arg, call = call)
}

do_s3_check <- function(
  x,
  x_modifier,
  type,
  type_msg,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_inherits(x, type, mode = "any", ..., arg = arg, call = call)

  if (x_modifier) {
    do_bare_s3_check(
      x,
      type,
      class(x)[1],
      ...,
      arg = arg,
      call = call
    )
  }
}

do_bare_s3_check <- function(x, type, class_x_first, ..., arg, call) {
  if (type != class_x_first) {
    msg <- format_inline(
      "{.arg {arg}} must be a bare {.cls {type}}, ",
      "but it is of class {.cls {class_x_first}}."
    )

    cli_abort(
      message = msg,
      ...,
      call = call
    )
  }
}
