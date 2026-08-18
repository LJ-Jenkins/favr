#' Check if an object is of a specific object-oriented programming type
#'
#' @description
#' Check that an object is an `S3`, `S4`, `S7`, or `R6` object.
#' @param x An object to check.
#' @param ... Additional arguments passed to [cli_abort()][cli::cli_abort]
#' which forwards unmatched arguments to [abort()][rlang::abort].
#' @param allow_null Whether `x` is allowed to be `NULL`.
#' @inheritParams rlang::args_error_context
#' @return
#' `NULL` invisibly if the check passes or an error if it fails.
#' @details
#' `S3` checks are performed by checking if the object has a class
#' attribute with [`is.object()`] and is not an `S4` object.
#'
#' `S4` checks are performed using [`isS4()`].
#'
#' `S7` and `R6` checks are performed by checking for the inheritance of
#' the `S7_object` and `R6` classes, respectively.
#' @name oop-checks
#' @family checks
#' @examples
#' check_s3(factor("a"))
#' check_s3(1:3) |> try()
#'
#' setClass("Person",
#'   slots = c(name = "character", age = "numeric")
#' )
#' x <- new("Person", name = "John", age = 30)
#'
#' check_s4(x)
#' check_s4(factor("a")) |> try()
#'
#' # trivial examples of inheritance checks for S7 and R6 objects
#' x <- structure(list(), class = "S7_object")
#' check_s7(x)
#' check_s7(factor("a")) |> try()
#'
#' x <- structure(list(), class = "R6")
#' check_r6(x)
#' check_r6(factor("a")) |> try()
NULL

#' @rdname oop-checks
#' @export
check_s3 <- function(
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

  if (!is.object(x) || isS4(x)) {
    cli_abort(
      message = format_inline(
        "{.arg {arg}} must be an {.cls S3} object, not {.cls {class(x)}}."
      ),
      ...,
      call = call
    )
  }

  invisible(NULL)
}

#' @rdname oop-checks
#' @export
check_s4 <- function(
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

  if (!isS4(x)) {
    cli_abort(
      message = format_inline(
        "{.arg {arg}} must be an {.cls S4} object, not {.cls {class(x)}}."
      ),
      ...,
      call = call
    )
  }

  invisible(NULL)
}

#' @rdname oop-checks
#' @export
check_s7 <- function(
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

  if (!inherits(x, "S7_object")) {
    cli_abort(
      message = check_class_err_msg(x, "S7_object", "any", arg),
      ...,
      call = call
    )
  }

  invisible(NULL)
}

#' @rdname oop-checks
#' @export
check_r6 <- function(
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

  if (!inherits(x, "R6")) {
    cli_abort(
      message = check_class_err_msg(x, "R6", "any", arg),
      ...,
      call = call
    )
  }

  invisible(NULL)
}
