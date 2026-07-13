#' File and directory existence checks
#'
#' @description
#' Check if inputs are existing directories or files and throw
#' an error if not.
#' @param x A path to check.
#' @param ... Additional arguments passed to [cli_abort()][cli::cli_abort]
#' which forwards unmatched arguments to [abort()][rlang::abort].
#' @inheritParams rlang::args_error_context
#' @return `NULL` invisibly if the check passes, otherwise an error is thrown.
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

    cli_abort(
      message = cli_fmt( # in case of odd `.envir`
        cli_bullets(
          c(
            "{.arg {arg}} must be an existing directory, but it {msg}.",
            "i" = "Path provided: {.path {x}}."
          )
        )
      ),
      ...,
      call = call
    )
  }
}

#' @rdname path-checks
#' @export
check_file <- function(x, ..., arg = caller_arg(x), call = caller_env()) {
  is_typed_path <- is.character(enexpr(x))

  check_string(x = x, ..., allow_empty = FALSE, arg = arg, call = call)

  isdir <- file.info(x, extra_cols = FALSE)[["isdir"]]

  if ((i <- is.na(isdir)) || isdir) {
    msg <- if (i) {
      "doesn't exist"
    } else {
      "is a directory"
    }

    if (is_typed_path) {
      arg <- "x"
    }

    cli_abort(
      message = cli_fmt(
        cli_bullets(
          c(
            "{.arg {arg}} must be an existing file, but it {msg}.",
            "i" = "Path provided: {.path {x}}."
          )
        )
      ),
      ...,
      call = call
    )
  }

  invisible(NULL)
}
