#' Array type checks
#'
#' @description
#' Check if inputs are expected types and throw an error if not.
#' @param x An object to check.
#' @param n,nrow,ncol The expected length, number of columns, or number of
#' rows of `x`.
#' @param ... Additional arguments passed to [cli_abort()][cli::cli_abort]
#' which forwards unmatched arguments to [abort()][rlang::abort].
#' @param finite Whether `x` is required to contain only finite values
#' (i.e. no `NA`, `Inf`, `-Inf`, or `NaN`).
#' @param allow_null Whether `x` is allowed to be `NULL`.
#' @inheritParams rlang::args_error_context
#' @return `NULL` invisibly if the check passes, otherwise an error is thrown.
#' @details
#' These functions can be used with the [bare()] modifier to check if an object
#' is a bare R object (i.e. has no class attribute), and the length modifiers
#' [at_least()], [at_most()], and [in_range()] to modify the behaviour of the
#' length checking `n`, `nrow`, and `ncol` arguments.
#' @note
#' These check functions are wrappers of their corresponding
#' base functions [is.array()] and [is.matrix()].
#' @name array-type-checks
#' @family checks
#' @examples
#' a <- array(1:12, dim = c(3, 4))
#' check_array(a)
#' check_array(1:12) |> try()
#'
#' m <- matrix(1:12, nrow = 3)
#' check_matrix(m)
#' check_matrix(1:12) |> try()
#'
#' class(m) <- c("my_matrix", class(m))
#' check_matrix(bare(m)) |> try()
#'
#' check_array(a, n = 10) |> try()
#' check_array(a, n = at_least(10))
#'
#' check_matrix(m, ncol = at_most(3)) |> try()
#' check_matrix(m, nrow = in_range(1, 10))
NULL

#' @rdname array-type-checks
#' @export
check_array <- function(
  x,
  n = NULL,
  nrow = NULL,
  ncol = NULL,
  ...,
  finite = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_array_types_impl(
    x,
    is.array,
    n = n,
    nrow = nrow,
    ncol = ncol,
    type = "array",
    type_msg = "an {.cls array}",
    ...,
    finite = finite,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

#' @rdname array-type-checks
#' @export
check_matrix <- function(
  x,
  n = NULL,
  nrow = NULL,
  ncol = NULL,
  ...,
  finite = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_array_types_impl(
    x,
    is.matrix,
    n = n,
    nrow = nrow,
    ncol = ncol,
    type = "matrix",
    type_msg = "a {.cls matrix}",
    ...,
    finite = finite,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

#' @rdname array-type-checks
#' @export
check_table <- function(
  x,
  n = NULL,
  nrow = NULL,
  ncol = NULL,
  ...,
  finite = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_array_types_impl(
    x,
    is.table,
    n = n,
    nrow = nrow,
    ncol = ncol,
    type = "table",
    type_msg = "a {.cls table}",
    ...,
    finite = finite,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_array_types_impl <- function(
  x,
  .fn,
  n,
  nrow,
  ncol,
  type,
  type_msg,
  ...,
  finite = FALSE,
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

  if (!.fn(x)) {
    cli_abort(
      message = wrong_type_msg(arg, type_msg, x),
      ...,
      call = call
    )
  }

  if (x_modifier) {
    do_bare_s3_check(
      x,
      type,
      class(x)[1],
      paste0("{.cls ", type, "}"),
      ...,
      arg = arg,
      call = call
    )
  }

  n_check(x, n, type_msg, ..., arg = arg, call = call)

  nrow_check(x, nrow, type_msg, ..., arg = arg, call = call)

  ncol_check(x, ncol, type_msg, ..., arg = arg, call = call)

  finite_check(finite, x, n, arg, ..., call = call)
}
