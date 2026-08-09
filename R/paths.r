#' File and directory existence checks
#'
#' @description
#' Check if inputs are existing directories or files and throw
#' an error if not.
#' @param x A path to check.
#' @param ext A character vector of file extensions to check for.
#' @param case A logical value indicating if the extension check should
#' be case-sensitive. If `FALSE`, the check will be case-insensitive.
#' @param ... Additional arguments passed to [cli_abort()][cli::cli_abort]
#' which forwards unmatched arguments to [abort()][rlang::abort].
#' @inheritParams rlang::args_error_context
#' @param arg,x_arg,ext_arg
#' An argument name as a string. This argument will be mentioned in
#' error messages as the input that is at the origin of a problem.
#' @param call
#' The execution environment of a currently running function, e.g.
#' `caller_env()`. The function will be mentioned in error messages as
#' the source of the error. See the `call` argument of [`abort()`] for
#' more information.
#' @return `NULL` invisibly if the check passes, otherwise an error is thrown.
#' @note The checking of extensions is done simply using [endsWith()].
#' @name path-checks
#' @family checks
#' @examples
#' x <- file.path(R.home(), "library", "stats")
#'
#' check_dir(x)
#' check_file(x) |> try()
#'
#' x <- file.path(x, "DESCRIPTION")
#'
#' check_file(x)
#' check_dir(x) |> try()
#' check_file(x, ext = c(".csv", ".xlsx")) |> try()
NULL

#' @rdname path-checks
#' @export
check_dir <- function(x, ..., arg = caller_arg(x), call = caller_env()) {
  is_typed_path <- is.character(enexpr(x))

  check_string(x = x, ..., allow_empty = FALSE, arg = arg, call = call)

  if (!dir.exists(x)) {
    msg <- if (file.exists(x)) {
      "is a file"
    } else {
      "doesn't exist"
    }

    if (is_typed_path) {
      arg <- "x"
    }

    cli_abort( # using `c_bull()` in case of odd `.envir`
      message = c_bull(
        "{.arg {arg}} must be an existing directory, but it {msg}.",
        "i" = "Path provided: {.path {x}}."
      ),
      ...,
      call = call
    )
  }
}

#' @rdname path-checks
#' @export
check_file <- function(
  x,
  ...,
  ext = NULL,
  case = TRUE,
  x_arg = caller_arg(x),
  ext_arg = caller_arg(ext),
  call = caller_env()
) {
  is_typed_path <- is.character(enexpr(x))

  check_string(x = x, ..., allow_empty = FALSE, arg = x_arg, call = call)

  if (!is.null(ext)) {
    .check_ext(
      x = x,
      ext = ext,
      ...,
      case = case,
      x_arg = x_arg,
      ext_arg = ext_arg,
      call = call
    )
  }

  isdir <- file.info(x, extra_cols = FALSE)[["isdir"]]

  if ((i <- is.na(isdir)) || isdir) {
    msg <- if (i) {
      "doesn't exist"
    } else {
      "is a directory"
    }

    if (is_typed_path) {
      x_arg <- "x"
    }

    cli_abort(
      message = c_bull(
        "{.arg {x_arg}} must be an existing file, but it {msg}.",
        "i" = "Path provided: {.path {x}}."
      ),
      ...,
      call = call
    )
  }

  invisible(NULL)
}

#' @rdname path-checks
#' @export
check_ext <- function(
  x,
  ext,
  ...,
  case = TRUE,
  x_arg = caller_arg(x),
  ext_arg = caller_arg(ext),
  call = caller_env()
) {
  check_string(x, ..., allow_empty = FALSE, arg = x_arg, call = call)

  .check_ext(
    x = x,
    ext = ext,
    ...,
    case = case,
    x_arg = x_arg,
    ext_arg = ext_arg,
    call = call
  )
}

.check_ext <- function(
  x,
  ext,
  ...,
  case = NULL,
  x_arg = NULL,
  ext_arg = NULL,
  call = NULL
) {
  check_character(
    ext,
    at_least(1),
    ...,
    allow_na = TRUE,
    arg = ext_arg,
    call = call
  )

  if (any(!nzchar(ext))) {
    cli_abort(
      message = "{.arg {ext_arg}} must not contain empty strings.",
      ...,
      call = call
    )
  }

  if (isFALSE(case)) {
    x <- tolower(x)
    ext <- tolower(ext)
  }

  if (any(endsWith(x, ext))) {
    return(invisible(NULL))
  }

  cli_abort(
    message = "{.arg {x_arg}} must have extension {.or {.val {ext}}}.",
    ...,
    call = call
  )
}
