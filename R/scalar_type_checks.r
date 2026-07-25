#' Scalar type checks
#'
#' @description
#' Check if inputs are scalars of an expected type and
#' throw an error if not.
#' @param x An object to check.
#' @param ... Additional arguments passed to [cli_abort()][cli::cli_abort]
#' which forwards unmatched arguments to [abort()][rlang::abort].
#' @param allow_na Whether `x` is allowed to
#' contain `NA` values.
#' @param allow_null Whether `x` is allowed to be `NULL`.
#' @param finite Whether `x` is required to contain only finite values
#' (i.e. no `NA`, `Inf`, `-Inf`, or `NaN`).
#' @inheritParams rlang::args_error_context
#' @return `NULL` invisibly if the check passes, otherwise an error is thrown.
#' @details
#' These functions can be used with the [bare()] modifier to check if an object
#' is a bare R object (i.e. has no class attribute).
#' @note
#' To handle empty strings (`""`) use [`check_string()`] instead of
#' `check_scalar_character()`.
#'
#' These check functions are wrappers of their corresponding
#' [rlang][rlang::scalar-type-predicates] functions. The exception
#' is `check_scalar_numeric()`, which uses [is.numeric()].
#' @name scalar-type-checks
#' @family checks
#' @examples
#' x <- 1L
#' check_scalar_integer(x)
#' check_scalar_double(x) |> try()
#'
#' check_scalar_list(list(list()))
#' check_scalar_list(list(1, 2)) |> try()
#'
#' check_scalar_character(NA_character_, allow_na = FALSE) |> try()
#' check_scalar_double(Inf, finite = TRUE) |> try()
#'
#' check_scalar_logical(NULL, allow_null = TRUE)
NULL

#' @rdname scalar-type-checks
#' @export
check_scalar_list <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_types_impl(
    is_list,
    "a scalar {.cls list}",
    x,
    n = 1L,
    ...,
    allow_na = TRUE,
    allow_null = allow_null,
    arg = arg,
    call = call,
    scalar = TRUE
  )
}

#' @rdname scalar-type-checks
#' @export
check_scalar_atomic <- function(
  x,
  ...,
  allow_na = TRUE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_types_impl(
    is_atomic,
    "a scalar {.cls atomic}",
    x,
    n = 1L,
    ...,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call,
    scalar = TRUE
  )
}

#' @rdname scalar-type-checks
#' @export
check_scalar_vector <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_types_impl(
    is_vector,
    "a scalar {.cls vector}",
    x,
    n = 1L,
    ...,
    allow_na = TRUE,
    allow_null = allow_null,
    arg = arg,
    call = call,
    scalar = TRUE
  )
}

#' @rdname scalar-type-checks
#' @export
check_scalar_integer <- function(
  x,
  ...,
  allow_na = TRUE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_types_impl(
    is_integer,
    "a scalar {.cls integer}",
    x,
    n = 1L,
    ...,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call,
    scalar = TRUE
  )
}

#' @rdname scalar-type-checks
#' @export
check_scalar_double <- function(
  x,
  ...,
  finite = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_types_impl(
    is_double,
    "a scalar {.cls double}",
    x,
    n = 1L,
    ...,
    allow_na = TRUE,
    allow_null = allow_null,
    arg = arg,
    call = call,
    scalar = TRUE
  )

  if (finite && !is.finite(x)) {
    cli_abort(
      message = non_finite_msg(arg, 1L, x),
      ...,
      call = call
    )
  }

  invisible(NULL)
}

#' @rdname scalar-type-checks
#' @export
check_scalar_complex <- function(
  x,
  ...,
  finite = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_types_impl(
    is_complex,
    "a scalar {.cls complex}",
    x,
    n = 1L,
    ...,
    allow_na = TRUE,
    allow_null = allow_null,
    arg = arg,
    call = call,
    scalar = TRUE
  )

  if (finite && !is.finite(x)) {
    cli_abort(
      message = non_finite_msg(arg, 1L, x),
      ...,
      call = call
    )
  }

  invisible(NULL)
}

#' @rdname scalar-type-checks
#' @export
check_scalar_character <- function(
  x,
  ...,
  allow_na = TRUE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_types_impl(
    is_character,
    "a scalar {.cls character}",
    x,
    n = 1L,
    ...,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call,
    scalar = TRUE
  )
}

#' @rdname scalar-type-checks
#' @export
check_scalar_logical <- function(
  x,
  ...,
  allow_na = TRUE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_types_impl(
    is_logical,
    "a scalar {.cls logical}",
    x,
    n = 1L,
    ...,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call,
    scalar = TRUE
  )
}

#' @rdname scalar-type-checks
#' @export
check_scalar_raw <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_types_impl(
    is_raw,
    "a scalar {.cls raw}",
    x,
    n = 1L,
    ...,
    allow_na = TRUE,
    allow_null = allow_null,
    arg = arg,
    call = call,
    scalar = TRUE
  )
}

#' @rdname scalar-type-checks
#' @export
check_scalar_bytes <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_types_impl(
    is_bytes,
    "a scalar {.cls bytes}",
    x,
    n = 1L,
    ...,
    allow_na = TRUE,
    allow_null = allow_null,
    arg = arg,
    call = call,
    scalar = TRUE
  )
}

#' @rdname scalar-type-checks
#' @export
check_scalar_numeric <- function(
  x,
  ...,
  finite = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  type <- "a scalar {.cls numeric}"

  if (inherits(x, "favr_modifier")) {
    arg <- x[["arg"]]
    do_bare_check(x, arg, type, ..., call = call)
    x <- x[["obj"]]
  }

  if (allow_null && is.null(x)) {
    return(invisible(NULL))
  }

  if (!is.numeric(x)) {
    cli_abort(
      message = wrong_type_msg(arg, type, x),
      ...,
      call = call
    )
  }

  if (length(x) != 1L) {
    cli_abort(
      message = wrong_scalar_length_msg(arg, type, x),
      ...,
      call = call
    )
  }

  if (finite && any(!is.finite(x))) {
    cli_abort(
      message = non_finite_msg(arg, 1L, x),
      ...,
      call = call
    )
  }

  invisible(NULL)
}
