#' Check class inheritance of an object
#'
#' @description
#' Check that an object inherits from a specific class (or classes)
#' and throw an error if not.
#' @param x An object to check.
#' @param class Character vector of class names to check against.
#' @param match The behaviour to use for inheritance checking. See Details.
#' @param ... Additional arguments passed to [cli_abort()][cli::cli_abort]
#' which forwards unmatched arguments to [abort()][rlang::abort].
#' @inheritParams rlang::args_error_context
#' @return
#' `NULL` invisibly if the check passes or an error if it fails.
#' @details
#' The `match` argument specifies how to check the inheritance:
#' - `"any"`: the class vector of `x` must have at least one element
#' in common with `class`.
#' - `"exact"`: the class vector of `x` must be identical to `class`.
#' - `"all"`: the class vector of `x` must contain all elements of
#' `class` in the supplied order.
#'
#' `check_class()` is a utility wrapper around `check_inherits()`
#' with `match = "exact"`.
#' @note These check functions are wrappers of their corresponding
#' [rlang][rlang::inherits_any] counterparts.
#' @name inheritance-checks
#' @family checks
#' @examples
#' # Default behaviour is to check for any inheritance.
#' x <- structure(1, class = c("a", "b", "c"))
#' check_inherits(x, c("x", "b", "y"))
#' check_inherits(x, c("x", "y")) |> try()
#'
#' # `match = "exact"` checks for exact match of the class vector.
#' x <- structure(1, class = c("a", "b", "c"))
#' check_inherits(x, c("a", "b", "c"), match = "exact")
#' check_inherits(x, c("a", "b")) |> try()
#'
#' # check_class() is a utility wrapper with match = "exact".
#' check_class(x, c("a", "b", "c"))
#' check_class(x, c("a", "b")) |> try()
#'
#' # `match = "all"` checks that inheritance is from all
#' # of the classes in the supplied order.
#' x <- structure(1, class = c("a", "b", "c", "d", "e"))
#' check_inherits(x, c("b", "d"), match = "all")
#' check_inherits(x, c("d", "b"), match = "all") |> try()
NULL

#' @rdname inheritance-checks
#' @export
check_inherits <- function(
  x,
  class,
  match = c("any", "exact", "all"),
  ...,
  arg = caller_arg(x),
  call = caller_env()
) {
  match <- arg_match0(
    match,
    c("any", "exact", "all"),
    error_call = call
  )

  check_character(class, n = at_least(1), ..., call = call)

  res <- switch(match,
    any = inherits(x, class),
    exact = identical(class(x), class),
    all = inherits_all(x, class)
  )

  if (!res) {
    cli_abort(
      message = check_class_err_msg(x, class, match, arg),
      ...,
      call = call
    )
  }

  invisible(NULL)
}

#' @rdname inheritance-checks
#' @export
check_class <- function(
  x,
  class,
  ...,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_character(class, n = at_least(1), ..., call = call)

  if (!identical(class(x), class)) {
    cli_abort(
      message = check_class_err_msg(x, class, "exact", arg),
      ...,
      call = call
    )
  }

  invisible(NULL)
}

check_class_err_msg <- function(x, target, match, arg) {
  # format here in case of `.envir` passed in dots.
  switch(match,
    any = format_inline(
      "{.var {arg}} must inherit from {if (length(target) > 1) 'any of '}",
      "{.cls {target}}, but is class {.cls {class(x)}}."
    ),
    exact = format_inline(
      "{.var {arg}} must be class {.cls {target}}",
      ", but is class {.cls {class(x)}}."
    ),
    all = format_inline(
      "{.var {arg}} must inherit from all of class {.cls {target}} in order",
      ", but is class {.cls {class(x)}}."
    )
  )
}
