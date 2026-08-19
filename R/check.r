#' Check the truth of tidy evaluated expressions
#'
#' @description
#' If any of the expressions in `...` are not ([`all`]) `TRUE`,
#' [`cli_abort()`][cli::cli_abort] is called, producing an error
#' message indicating the first expression which was not ([`all`])
#' `TRUE`.
#'
#' `check_with()` is a data-masked version of `check()`,
#' evaluating the expression in the context of `.data`.
#' @param ... Any number of R expressions, which should each evaluate to
#' (a [`logical`] vector of **all**) [`TRUE`] for no error to occur.
#' Non-`logical` and `NA` values will trigger an error.
#'
#' If an expression is named, the name will be used in the error message
#' instead of the default message or the `message` argument.
#' @param .data A data frame, list, or environment to evaluate the
#' expressions in as a data mask.
#' @param message Default error message for non-named expressions.
#' @param call An execution environment, defused function call, or `NULL`.
#' Passed to [`cli_abort()`][cli::cli_abort].
#' @param .envir Environment to evaluate the cli formatting of the error
#' message in. Passed to [`cli_abort()`][cli::cli_abort].
#'
#' For `check_with()`,  the messages are evaluated in the context of
#' `.data` and `.envir`. See examples.
#' @param .frame The throwing context. Passed to
#' [`cli_abort()`][cli::cli_abort].
#' @param abort_args A list of additional arguments to pass to
#' [`abort()`][rlang::abort] (forwarded from [`cli_abort()`][cli::cli_abort]).
#' @return `NULL`, called for side effects only.
#' @seealso
#' [`abortifnot()`] for a more performant version without tidy evaluation and
#' [injection][topic-inject] support.
#' @examples
#' check(1 == 1, all.equal(pi, 3.14159265), 1 < 2) # all TRUE
#'
#' data <- data.frame(x = 1:5, y = 6:10)
#' check_with(data, x < y, is.numeric(x), length(y) < 10) # all TRUE
#'
#' # A custom error message can be given for each
#' # expression, with cli formatting.
#' check(
#'   "message {.arg 1}" = TRUE, "message {.arg 2}" = FALSE
#' ) |> try()
#'
#' # check_with() names are also are evaluated in
#' # the context of `.data` then `.envir`.
#' x <- "env 'x'"
#' y <- "env 'y'"
#' data <- list(x = "data 'x'")
#' check_with(data, "{x}" = is.numeric(x)) |>
#'   try()
#' check_with(data, "{y}" = is.numeric(x)) |>
#'   try()
#'
#' # Pronouns are supported in check_with() error
#' # messages, but must be spaced according to cli
#' # rules (e.g., use `{ .env$x}` instead of `{.env$x}`).
#' check_with(data, "{ .env$x}" = is.numeric(x)) |>
#'   try()
#'
#' # Alternatively, one error message can be used for all
#' # expressions.
#' x <- 1:3
#' check(
#'   x > 0, x < 3,
#'   message = "{.arg x} has incorrect values: {.val {x}}."
#' ) |> try()
#'
#' data <- data.frame(x = c("a", "b", "c"))
#' check_with(data,
#'   is.numeric(x),
#'   message = "{.arg x} is not numeric: {.val {x}}."
#' ) |>
#'   try()
#'
#' # The `call` argument can be used to specify where the
#' # error occurs, by default this is the caller environment.
#' myfunc <- function(x) check(x)
#' myfunc(FALSE) |> try()
#'
#' myfunc_with <- function(x, ...) check_with(x, ...)
#' myfunc_with(list(x = 1), x < 0) |> try()
#'
#' # check() and check_with() error if any argument does
#' # not evaluate to (all) FALSE.
#' check(c(T, F)) |> try()
#' check_with(list(x = c(T, NA)), x) |>
#'   try()
#' @export
check <- function(
  ...,
  message = NULL,
  call = .envir,
  .envir = parent.frame(),
  .frame = .envir,
  abort_args = NULL
) {
  check_impl(
    ...,
    .data = NULL,
    message = message,
    call = call,
    .envir = .envir,
    .frame = .frame,
    abort_args = abort_args
  )
}

#' @rdname check
#' @export
check_with <- function(
  .data,
  ...,
  message = NULL,
  call = .envir,
  .envir = parent.frame(),
  .frame = .envir,
  abort_args = NULL
) {
  check_impl(
    ...,
    .data = .data,
    message = message,
    call = call,
    .envir = .envir,
    .frame = .frame,
    abort_args = abort_args
  )

  invisible(.data)
}

check_impl <- function(
  ...,
  .data = .data,
  message = message,
  call = call,
  .envir = .envir,
  .frame = .frame,
  abort_args = abort_args
) {
  args <- enquos(...)

  le <- length(args)
  if (le == 0L) {
    return(invisible(NULL))
  }

  arg_names <- names(args)

  for (i in seq_len(le)) {
    x <- eval_tidy(args[[i]], data = .data)
    user_msg <- arg_names[i]

    if (!is.logical(x)) {
      abort_check_dot_arg(
        .data = .data,
        val = x,
        arg = args[[i]],
        # format eagerly with `.data`.
        arg_msg = user_msg,
        message = message,
        problem = "type",
        call = call,
        .envir = .envir,
        .frame = .frame,
        abort_args = abort_args
      )
    }

    if (anyNA(x)) {
      abort_check_dot_arg(
        .data = .data,
        val = x,
        arg = args[[i]],
        arg_msg = user_msg,
        message = message,
        problem = "na",
        call = call,
        .envir = .envir,
        .frame = .frame,
        abort_args = abort_args
      )
    }

    if (!all(x)) {
      abort_check_dot_arg(
        .data = .data,
        val = x,
        arg = args[[i]],
        arg_msg = user_msg,
        message = message,
        problem = "failed",
        call = call,
        .envir = .envir,
        .frame = .frame,
        abort_args = abort_args
      )
    }
  }

  invisible(NULL)
}

abort_check_dot_arg <- function(
  .data,
  val,
  arg,
  arg_msg,
  message,
  problem,
  call,
  .envir,
  .frame,
  abort_args,
  not = TRUE
) {
  arg_msg <- if (!is.null(arg_msg) && nzchar(arg_msg)) {
    do_masked_msg(arg_msg, .data, .envir)
  } else if (!is.null(message)) {
    do_masked_msg(message, .data, .envir)
  } else {
    do_internal_msg(as_label(arg), val, problem, not)
  }

  do_abort_args_err(arg_msg, call, .envir, .frame, abort_args)
}

do_masked_msg <- function(msg, .data, .envir) {
  if (!is.null(.data)) {
    eval_tidy(
      call2(format_inline, msg),
      data = .data,
      env = .envir
    )
  } else {
    format_inline(msg, .envir = .envir)
  }
}
