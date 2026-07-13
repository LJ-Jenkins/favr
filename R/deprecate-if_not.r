#' Ensure the truth of R expressions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions were deprecated in favour of [check()], [abortifnot()]
#' and [abortif()].
#'
#' If any of the expressions in `...` are not all `TRUE`, [abort][rlang::abort]
#' is called for the first expression which was not ([all]) `TRUE`. The names
#' of expressions can be used as the error message or a single default error
#' message can be given using `.message`. Both are passed to
#' [format_inline][cli::format_inline] for formatting.
#'
#' @param ... any number of R expressions, which should each evaluate to
#' (a logical vector of [all]) `TRUE` for no error to occur. Positive numbers
#' are not `TRUE`, even when they are coerced to `TRUE` inside `if()` or in
#' arithmetic computations in R. If the expressions are named, the names
#' will be used in the error message.
#' @param .message single default error message for non-named expressions.
#' @param .error_call the call environment to use for error messages
#' (passed to [abort][rlang::abort]).
#' @return NULL, called for side effects only.
#' @details [favr::abort_if] is the opposite of [favr::abort_if_not],
#' i.e. expressions should evaluate to ([all]) `FALSE` for no error to
#' occur. See [favr::enforce] and [favr::schema] for a non data-masked
#' and data-masked version of [favr::abort_if_not] with options for size
#' recycling and type casting.
#' @examples
#' # NB: Some of these examples are expected to produce an error. To
#' #     prevent them from terminating a run with example() they are
#' #     piped into a call to try().
#'
#' abort_if_not(1 == 1, all.equal(pi, 3.14159265), 1 < 2) # all TRUE
#'
#' m <- matrix(c(1, 3, 3, 1), 2, 2)
#' abort_if_not(m == t(m), diag(m) == rep(1, 2)) # all TRUE
#'
#' abort_if_not(1) |> try()
#'
#' # A custom error message can be given for each expression:
#' m[1, 2] <- 12
#' abort_if_not("{.var m} must be {.cls symmetric}" = m == t(m)) |>
#'   try()
#'
#' # Alternatively, one error message can be used for all
#' # expressions:
#' abort_if_not(
#'   m[1, 1] == 1,
#'   diag(m) == rep(2, 2),
#'   .message = "{.var m} has a diagonal of: {diag(m)}"
#' ) |> try()
#'
#' # The `.error_call` argument can be used to specify where the
#' # error occurs, by default this is the caller environment:
#' myfunc <- function(x) abort_if_not(x)
#' myfunc(FALSE) |> try()
#'
#' # abort_if() errors if any argument does not evaluate to
#' # (all) FALSE:
#' abort_if(1 == 1) |> try()
#'
#' # Injection can be used:
#' x <- "my error"
#' abort_if_not({{ x }} := FALSE) |> try()
#' abort_if_not(!!x := FALSE) |> try()
#' abort_if_not(FALSE, .message = "{x}") |> try()
#'
#' x <- list("my {.var bang-bang-bang} error" = FALSE)
#' abort_if_not(!!!x) |> try()
#' @keywords internal
#' @export
abort_if_not <- function(
  ...,
  .message = NULL,
  .error_call = caller_env()
) {
  lifecycle::deprecate_soft(
    when = "1.1.0",
    what = "abort_if_not()",
    details = "Please use `check()` or `abortifnot()` instead."
  )

  abort_dispatch(
    ...,
    .fn = check_logi_true,
    .message = .message,
    .error_call = .error_call,
    .caller_fn = "abort_if_not"
  )
}

#' @rdname abort_if_not
#' @keywords internal
#' @export
abort_if <- function(
  ...,
  .message = NULL,
  .error_call = caller_env()
) {
  lifecycle::deprecate_soft(
    when = "1.1.0",
    what = "abort_if()",
    details = "Please use `check()` or `abortif()` instead."
  )

  abort_dispatch(
    ...,
    .fn = check_logi_false,
    .message = .message,
    .error_call = .error_call,
    .caller_fn = "abort_if"
  )
}

abort_dispatch <- function(..., .fn, .message, .error_call, .caller_fn) {
  args <- enquos(...)
  eval_env <- caller_env(2)
  check_env(.error_call, call = eval_env, caller_fn = .caller_fn)

  if (!is.null(.message)) {
    check_character(.message, call = .error_call, caller_fn = .caller_fn)
    .message <- format_inline(.message, .envir = eval_env)
  }

  error_names <- vec_format_inline(
    names2(args),
    eval_env = eval_env
  )

  withCallingHandlers(
    for (i in seq_along(args)) {
      logi <- eval_tidy(args[[i]], env = eval_env)

      if (!is.logical(logi)) {
        abort_not_class(expected = "logical", given = class(logi))
      }

      .fn(logi)
    },
    error = function(cnd) {
      favr_error_handler(
        cnd = cnd,
        caller_fn = .caller_fn,
        expr = as_label(args[[i]]),
        msg = error_names[i] %""% .message,
        call = .error_call
      )
    }
  )
}

#' Cast objects to a given type
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated as there is rarely a need to
#' use it over [vctrs::vec_cast()].
#'
#' The names of the `...` expressions, which should be variables within
#' the `.env` envrionment, are attempted to be casted to the type specified
#' in the expression:
#' e.g., `name_of_object_to_cast = object_of_type_to_cast_to`. Expressions
#' are evaluated in the environment specified and objects are assigned back
#' into that same environment. Lossy casting can be undertaken by wrapping the
#' expression in a call to [favr::lossy], e.g., `x = lossy(integer())`. The
#' type conversion is from the [vctrs](https://vctrs.r-lib.org/) package
#' and thus sticks to the [vctrs type conversion rules](https://vctrs.r-lib.org/reference/faq-compatibility-types.html).
#'
#' @param ... any number of named R expressions.
#' @param .env the environment to use for the evaluation of the casting
#' expressions and the assignment of the casted objects.
#' @param .error_call the call environment to use for error messages
#' (passed to [abort][rlang::abort]).
#' @return NULL, but objects named in `...` will be changed in the
#' `.env` environment specified.
#' @details See [favr::abort_if_not] for general validation,
#' [favr::recycle_if_not] for recycling, and [favr::enforce] and [favr::schema]
#' for non data-masked and data-masked validations, recycling and casting.
#' @examples
#' # NB: Some of these examples are expected to produce an error. To
#' #     prevent them from terminating a run with example() they are
#' #     piped into a call to try().
#'
#' x <- 1L
#' cast_if_not(x = double())
#' class(x) # numeric
#'
#' # By default, lossy casting is not allowed:
#' x <- c(1, 1.5)
#' cast_if_not(x = integer()) |> try()
#'
#' # lossy casting can be enabled using `lossy()` call:
#' cast_if_not(x = lossy(integer()))
#' class(x) # integer
#'
#' # Other objects can be used as the type to cast to, e.g.:
#' x <- 1L
#' y <- 2.3
#' cast_if_not(x = y)
#' class(x) # numeric
#'
#' # Changed objects are available immediately:
#' x <- y <- 1L
#' cast_if_not(x = double(), y = x)
#' cat(class(x), class(y), sep = ", ") # numeric, numeric
#'
#' myfunc <- function(x) {
#'   cast_if_not(x = double())
#'   class(x)
#' }
#' x <- 1L
#' myfunc(x) # x is cast to double within the function
#' class(x) # x is still an integer outside the function
#'
#' # The `.env` argument determines the expression and assignment
#' # environment:
#' x <- 1L
#' e <- new.env()
#' e$x <- 1L
#' cast_if_not(x = 1.5, .env = e)
#' cat(
#'   "environment 'e'", class(e$x), "local environment", class(x),
#'   sep = ", "
#' ) # environment 'e', numeric, local environment, integer
#'
#' # Named objects (lhs) are checked to be in the `.env` environment,
#' # throwing an error if not found:
#' x <- 1L
#' e <- new.env()
#' cast_if_not(x = 1.5, .env = e) |> try()
#'
#' # For expressions (rhs), the `.env` argument is preferentially
#' # chosen, but if not found then the normal R scoping rules
#' # apply:
#' x <- 1.5
#' e <- new.env()
#' e$z <- 1L
#' cast_if_not(z = x, .env = e)
#' class(e$z) # numeric
#'
#' # The `.error_call` argument can be used to specify where the
#' # error occurs, by default this is the caller environment:
#' myfunc <- function(x) cast_if_not(x = character())
#' myfunc(FALSE) |> try()
#'
#' # Injection can be used:
#' y <- 1L
#' x <- "y"
#' cast_if_not(!!x := double()) |> try()
#' class(y) # numeric
#'
#' y <- 1L
#' x <- list(y = double())
#' cast_if_not(!!!x)
#' class(y) # numeric
#'
#' # Objects are reverted to their original values if an error
#' # occur:
#' x <- y <- 1L
#' cast_if_not(x = double(), y = character()) |> try()
#' class(x) # integer
#' @export
cast_if_not <- function(
  ...,
  .env = caller_env(),
  .error_call = caller_env()
) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "cast_if_not()",
    with = "vctrs::vec_cast()"
  )

  args <- enquos(...)
  caller_fn <- "cast_if_not"

  check_env(.error_call, call = caller_env(), caller_fn = caller_fn)
  check_env(.env, call = .error_call, caller_fn = caller_fn)

  vars <- names2(args)
  check_args_named_and_vars_exist(
    vars,
    .env,
    action = "cast",
    call = caller_env(),
    caller_fn = caller_fn
  )

  add_caller_fn(caller_fn)

  withCallingHandlers(
    for (i in seq_along(args)) {
      .favr_cast_to_type <- eval_tidy(args[[i]], env = .env)

      collect_old_value(vars[i], .env)

      # used to test first using vctrs::vec_is, but it is faster
      # to just use vec_cast directly even if already that type
      vctrs_cast_call <- call2(
        vec_cast_lossy,
        sym(vars[i]),
        .favr_cast_to_type,
        x_arg = vars[i],
        allow_lossy = attr(.favr_cast_to_type, "favr:::lossy")
      )

      assign(
        x = vars[i],
        value = eval_tidy(vctrs_cast_call, env = .env),
        pos = .env
      )
    },
    error = function(cnd) {
      restore_old_values(.env)

      favr_error_handler(
        cnd = cnd,
        caller_fn = caller_fn,
        expr = paste(vars[i], "=", as_label(args[[i]])),
        call = .error_call
      )
    }
  )

  clean_favr_env_on_exit()
}

#' Recycle objects to a given size
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated as there is rarely a need to
#' use it over [vctrs::vec_recycle()].
#'
#' The names of the `...` expressions, which should be variables within
#' the `.env` envrionment, are attempted to be recycled to the size specified
#' in the expression:
#' e.g., `name_of_object_to_recycle = size_to_recycle_to`. Expressions
#' are evaluated in the environment specified and objects are assigned back
#' into that same environment. The object recycling is from the
#' [vctrs](https://vctrs.r-lib.org/) package and thus stick to the
#' [vctrs recycling rules](https://vctrs.r-lib.org/reference/theory-faq-recycling.html).
#'
#' @param ... any number of named R expressions.
#' @param .env the environment to use for the evaluation of the recycling
#' expressions and the assignment of the recycled objects.
#' @param .error_call the call environment to use for error messages
#' (passed to [abort][rlang::abort]).
#' @return NULL, but objects named in `...` will be changed in the
#' `.env` environment specified.
#' @details See [favr::abort_if_not] for general validation,
#' [favr::recycle_if_not] for recycling, and [favr::enforce] and [favr::schema]
#' for non data-masked and data-masked validations, recycling and casting.
#' @examples
#' # NB: Some of these examples are expected to produce an error. To
#' #     prevent them from terminating a run with example() they are
#' #     piped into a call to try().
#'
#' x <- 1
#' recycle_if_not(x = 5)
#' length(x) # 5
#'
#' # recycle_if_not() follows `vctrs` recycling rules:
#' x <- c(1, 1)
#' recycle_if_not(x = 6) |> try()
#'
#' # Beware when using other objects as the size argument, e.g.:
#' x <- 1L
#' y <- c(1, 1, 1)
#' recycle_if_not(x = y) |> try()
#'
#' # When using other objects, call vctrs::vec_size() on them first:
#' recycle_if_not(x = vctrs::vec_size(y))
#' length(x) # 3
#'
#' # Changed objects are available immediately:
#' x <- y <- 1
#' recycle_if_not(x = 3, y = vctrs::vec_size(x))
#' cat(length(x), length(y), sep = ", ") # 3, 3
#'
#' myfunc <- function(x) {
#'   recycle_if_not(x = 3)
#'   length(x)
#' }
#' x <- 1L
#' myfunc(x) # x is recycled to length 3 within the function
#' length(x) # x is still scalar outside the function
#'
#' # The `.env` argument determines the expression and assignment
#' # environment:
#' x <- 1
#' e <- new.env()
#' e$x <- 1
#' recycle_if_not(x = 3, .env = e)
#' cat(
#'   "environment 'e'", length(e$x), "local environment", length(x),
#'   sep = ", "
#' ) # environment 'e', 3, local environment, 1
#'
#' # Named objects (lhs) are checked to be in the `.env` environment,
#' # throwing an error if not found:
#' x <- 1
#' e <- new.env()
#' recycle_if_not(x = 3, .env = e) |> try()
#'
#' # For expressions (rhs), the `.env` argument is preferentially
#' # chosen, but if not found then the normal R scoping rules
#' # apply:
#' x <- 3
#' e <- new.env()
#' e$z <- 1
#' recycle_if_not(z = x, .env = e)
#' length(e$z) # 3
#'
#' # The `.error_call` argument can be used to specify where the
#' # error occurs, by default this is the caller environment:
#' myfunc <- function(x) recycle_if_not(x = -5)
#' myfunc(1) |> try()
#'
#' #' # Injection can be used:
#' y <- 1L
#' x <- "y"
#' recycle_if_not(!!x := 5) |> try()
#' length(y) # 5
#'
#' y <- 1L
#' x <- list(y = 5)
#' recycle_if_not(!!!x)
#' length(y) # 5
#'
#' # Objects are reverted to their original values if an error
#' # occur:
#' x <- y <- 1L
#' recycle_if_not(x = 5, y = -5) |> try()
#' length(x) # 1
#' @export
recycle_if_not <- function(
  ...,
  .env = caller_env(),
  .error_call = caller_env()
) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "recycle_if_not()",
    with = "vctrs::vec_recycle()"
  )

  args <- enquos(...)
  caller_fn <- "recycle_if_not"

  check_env(.error_call, call = caller_env(), caller_fn = caller_fn)
  check_env(.env, call = .error_call, caller_fn = caller_fn)

  vars <- names2(args)
  check_args_named_and_vars_exist(
    vars,
    .env,
    action = "recycle",
    call = caller_env(),
    caller_fn = caller_fn
  )

  withCallingHandlers(
    for (i in seq_along(args)) {
      .favr_size_to_type <- eval_tidy(args[[i]], env = .env)

      collect_old_value(vars[i], .env)

      check_positive_scalar_integerish(.favr_size_to_type)

      # used to test first using vctrs::vec_size, but it is faster
      # to just use vec_recycle directly even if already that size
      vctrs_recycle_call <- call2(
        vec_recycle,
        sym(vars[i]),
        .favr_size_to_type,
        x_arg = vars[i]
      )

      assign(
        x = vars[i],
        value = eval_tidy(vctrs_recycle_call, env = .env),
        pos = .env
      )
    },
    error = function(cnd) {
      restore_old_values(.env)

      favr_error_handler(
        cnd = cnd,
        caller_fn = caller_fn,
        expr = paste(vars[i], "=", as_label(args[[i]])),
        call = .error_call
      )
    }
  )

  clean_favr_env_on_exit()
}
