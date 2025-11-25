#' Ensure the truth of R expressions
#'
#' If any of the expressions in `...` are not all `TRUE`, [rlang::abort] is
#' called for the first expression which was not ([all]) `TRUE`. The names
#' of expressions can be used as the error message or a single default error
#' message can be given using `.message`. Both are passed to
#' [cli::format_inline] for formatting.
#'
#' @param ... any number of R expressions, which should each evaluate to
#' (a logical vector of [all]) `TRUE` for no error to occur. Positive numbers
#' are not `TRUE`, even when they are coerced to `TRUE` inside `if()` or in
#' arithmetic computations in R. If the expressions are named, the names
#' will be used in the error message.
#' @param .message single default error message for non-named expressions.
#' @param .error_call the call environment to use for error messages
#' (passed to [rlang::abort]).
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
#' # Error:
#' # Caused by error in `abort_if_not()`:
#' # ℹ In argument: `1`.
#' # ! Argument must be <logical>, not <numeric>.
#'
#' # A custom error message can be given for each expression:
#' m[1, 2] <- 12
#' abort_if_not("{.var m} must be {.cls symmetric}" = m == t(m)) |>
#'   try()
#' # Error:
#' # Caused by error in `abort_if_not()`:
#' # ℹ In argument: `m == t(m)`.
#' # ! `m` must be <symmetric>
#'
#' # Alternatively, one error message can be used for all
#' # expressions:
#' abort_if_not(
#'   m[1, 1] == 1,
#'   diag(m) == rep(2, 2),
#'   .message = "{.var m} has a diagonal of: {diag(m)}"
#' ) |> try()
#' # Error:
#' # Caused by error in `abort_if_not()`:
#' # ℹ In argument: `diag(m) == rep(2, 2)`.
#' # ! `m` must have a diagonal of: 1 and 1
#'
#' # The `.error_call` argument can be used to specify where the
#' # error occurs, by default this is the caller environment:
#' myfunc <- function(x) abort_if_not(x)
#' myfunc(FALSE) |> try()
#' # Error in `myfunc()`:
#' # Caused by error in `abort_if_not()`:
#' # ℹ In argument: `x`.
#' # ! Returned `FALSE`.
#'
#' # abort_if() errors if any argument does not evaluate to
#' # (all) FALSE:
#' abort_if(1 == 1) |> try()
#' # Error:
#' # Caused by error in `abort_if()`:
#' # ℹ In argument: `1 == 1`.
#' # ! Returned `TRUE`.
#'
#' # Injection can be used:
#' x <- "my error"
#' abort_if_not({{ x }} := FALSE) |> try()
#' abort_if_not(!!x := FALSE) |> try()
#' abort_if_not(FALSE, .message = "{x}") |> try()
#' # Error:
#' # Caused by error in `abort_if_not()`:
#' # ℹ In argument: `FALSE`.
#' # ! my error
#' x <- list("my {.var bang-bang-bang} error" = FALSE)
#' abort_if_not(!!!x) |> try()
#' # Error:
#' # Caused by error in `abort_if_not()`:
#' # ℹ In argument: `FALSE`.
#' # ! my `bang-bang-bang` error
#' @export
abort_if_not <- function(
    ...,
    .message = NULL,
    .error_call = caller_env()) {
  abort_dispatch(
    ...,
    .fn = check_logi_true,
    .message = .message,
    .error_call = .error_call,
    .caller_fn = "abort_if_not"
  )
}

#' @rdname abort_if_not
#' @export
abort_if <- function(
    ...,
    .message = NULL,
    .error_call = caller_env()) {
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

#--
