#' Type checks
#'
#' @description
#' Check if inputs are expected types and throw an error if not.
#' @param x An object to check.
#' @param n The expected length of `x`.
#' @param ... Additional arguments passed to [cli_abort()][cli::cli_abort]
#' which forwards unmatched arguments to [abort()][rlang::abort].
#' @param allow_na Whether `x` is allowed to contain `NA` values.
#' @param allow_null Whether `x` is allowed to be `NULL`.
#' @param finite Whether `x` is required to contain only finite values
#' (i.e. no `NA`, `Inf`, `-Inf`, or `NaN`).
#' @inheritParams rlang::args_error_context
#' @return `NULL` invisibly if the check passes, otherwise an error is thrown.
#' @details
#' These functions can be used with the [bare()] modifier to check if an object
#' is a bare R object (i.e. has no class attribute), and the length modifiers
#' [at_least()], [at_most()], and [in_range()] to modify the behaviour of the
#' length checking `n` argument.
#' @note
#' `check_null()` cannot use `bare()` since `NULL` cannot have a class
#' attribute.
#'
#' These check functions are wrappers of their corresponding
#' [rlang][rlang::type-predicates] functions. The exception is
#' `check_numeric()`, which uses [is.numeric()].
#' @name type-checks
#' @family checks
#' @examples
#' x <- c(1, 2, 3)
#'
#' check_integer(x) |> try()
#' check_integerish(x)
#' check_scalar_double(x) |> try()
#' check_double(x, n = 2) |> try()
#' check_double(x, n = at_least(4)) |> try()
#' check_double(x, n = at_most(2)) |> try()
#' check_double(x, n = in_range(1, 2)) |> try()
#'
#' check_integer(bare(factor(1))) |> try()
#'
#' check_double(c(1L, NA), allow_na = FALSE) |> try()
#' check_double(c(1.5, NA), finite = TRUE) |> try()
#'
#' check_double(NULL, allow_null = TRUE)
#'
#' # NULL list elements are not considered NULL
#' check_list(list(NULL), allow_null = TRUE) |> try()
NULL

#' @rdname type-checks
#' @export
check_list <- function(
  x,
  n = NULL,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_types_impl(
    is_list,
    "a {.cls list}",
    x,
    n = n,
    ...,
    allow_na = TRUE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

#' @rdname type-checks
#' @export
check_atomic <- function(
  x,
  n = NULL,
  ...,
  allow_na = TRUE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_types_impl(
    is_atomic,
    "an {.cls atomic} vector",
    x,
    n = n,
    ...,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

#' @rdname type-checks
#' @export
check_vector <- function(
  x,
  n = NULL,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_types_impl(
    is_vector,
    "a {.cls vector}",
    x,
    n = n,
    ...,
    allow_na = TRUE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

#' @rdname type-checks
#' @export
check_integer <- function(
  x,
  n = NULL,
  ...,
  allow_na = TRUE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_types_impl(
    is_integer,
    "an {.cls integer} vector",
    x,
    n = n,
    ...,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

#' @rdname type-checks
#' @export
check_integerish <- function(
  x,
  n = NULL,
  ...,
  finite = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_types_impl(
    is_integerish,
    "an {.cls integer}'ish' vector",
    x,
    n = n,
    ...,
    allow_na = TRUE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )

  finite_check(finite, x, n, arg, ..., call = call)
}

#' @rdname type-checks
#' @export
check_double <- function(
  x,
  n = NULL,
  ...,
  finite = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_types_impl(
    is_double,
    "a {.cls double} vector",
    x,
    n = n,
    ...,
    allow_na = TRUE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )

  finite_check(finite, x, n, arg, ..., call = call)
}

#' @rdname type-checks
#' @export
check_complex <- function(
  x,
  n = NULL,
  ...,
  finite = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_types_impl(
    is_complex,
    "a {.cls complex} vector",
    x,
    n = n,
    ...,
    allow_na = TRUE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )

  finite_check(finite, x, n, arg, ..., call = call)
}

#' @rdname type-checks
#' @export
check_character <- function(
  x,
  n = NULL,
  ...,
  allow_na = TRUE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_types_impl(
    is_character,
    "a {.cls character} vector",
    x,
    n = n,
    ...,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

#' @rdname type-checks
#' @export
check_logical <- function(
  x,
  n = NULL,
  ...,
  allow_na = TRUE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_types_impl(
    is_logical,
    "a {.cls logical} vector",
    x,
    n = n,
    ...,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

#' @rdname type-checks
#' @export
check_raw <- function(
  x,
  n = NULL,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_types_impl(
    is_raw,
    "a {.cls raw} vector",
    x,
    n = n,
    ...,
    allow_na = TRUE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

#' @rdname type-checks
#' @export
check_bytes <- function(
  x,
  n = NULL,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_types_impl(
    is_bytes,
    "a {.cls bytes} vector",
    x,
    n = n,
    ...,
    allow_na = TRUE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

#' @rdname type-checks
#' @export
check_null <- function(
  x,
  ...,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!is.null(x)) {
    cli_abort(
      message = wrong_type_msg(arg, "{.cls NULL}", x),
      ...,
      call = call
    )
  }

  invisible(NULL)
}

#' @rdname type-checks
#' @export
check_numeric <- function(
  x,
  n = NULL,
  ...,
  finite = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  # special case as not rlang function with builtin 'n'
  type <- "a {.cls numeric} vector"

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

  n_check(x, n, type, ..., arg = arg, call = call)

  finite_check(finite, x, n, arg, ..., call = call)
}

check_types_impl <- function(
  .fn,
  type,
  x,
  n,
  ...,
  allow_na = TRUE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env(),
  scalar = FALSE
) {
  if (inherits(x, "favr_modifier")) {
    arg <- x[["arg"]]
    do_bare_check(x, arg, type, ..., call = call)
    x <- x[["obj"]]
  }

  if (allow_null && is.null(x)) {
    return(invisible(NULL))
  }

  if (inherits(n, "favr_modifier")) {
    do_length_modifier_check(.fn, type, x, n, ..., arg = arg, call = call)
  } else {
    if (!.fn(x, n = n)) {
      type_issue <- if (is.null(n)) {
        TRUE
      } else {
        !.fn(x, n = NULL)
      }

      msg <- if (type_issue) {
        wrong_type_msg(arg, type, x)
      } else {
        msg <- if (scalar) {
          wrong_scalar_length_msg(arg, type, x)
        } else {
          wrong_length_msg(arg, type, n, x)
        }
      }

      cli_abort(
        message = msg,
        ...,
        call = call
      )
    }
  }

  na_check(allow_na, x, n, arg, ..., call = call)
}

do_length_modifier_check <- function(
  .fn,
  type,
  x,
  n,
  ...,
  arg = NULL,
  call = NULL
) {
  if (!.fn(x, n = NULL)) {
    cli_abort(
      message = wrong_type_msg(arg, type, x),
      ...,
      call = call
    )
  }

  # already validated `n` not null
  do_n_check(x, n, type, ..., arg = arg, call = call)
}
