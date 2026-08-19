#' Ensure the truth of R expressions
#'
#' @description
#' If any of the expressions in `...` are not ([`all`]) `TRUE`,
#' [`cli_abort()`][cli::cli_abort] is called, producing an error
#' message indicating the first expression which was not ([`all`])
#' `TRUE`.
#'
#' For `abortif()`, the opposite is true, i.e. expressions
#' should evaluate to ([`all`]) `FALSE` for no error to occur.
#' @param ... Any number of R expressions, which should each evaluate to
#' (a [`logical`] vector of **all**) [`TRUE`] for no error to occur (`FALSE`
#' for `abortif()`). Non-`logical` and `NA` values will trigger an error.
#'
#' If an expression is named, the name will be used in the error message
#' instead of the default message or the `message` argument.
#' @param message Default error message for non-named expressions.
#' @param call An execution environment, defused function call, or `NULL`.
#' Passed to [`cli_abort()`][cli::cli_abort].
#' @param .envir Environment to evaluate the cli formatting of the error
#' message in. Passed to [`cli_abort()`][cli::cli_abort].
#' @param .frame The throwing context. Passed to
#' [`cli_abort()`][cli::cli_abort].
#' @param abort_args A list of additional arguments to pass to
#' [`abort()`][rlang::abort] (forwarded from [`cli_abort()`][cli::cli_abort]).
#' @return `NULL` invisibly if the checks pass, otherwise an error is thrown.
#' @seealso
#' [`stopifnot()`] for the base **R** function this is based on.
#'
#' [`check()`] and [`check_with()`] for a non data-masked
#' and data-masked version of `abortifnot()` with tidy evaluation and
#' [injection][topic-inject] support.
#' @examples
#' abortifnot(1 == 1, all.equal(pi, 3.14159265), 1 < 2) # all TRUE
#'
#' m <- matrix(c(1, 3, 3, 1), 2, 2)
#' abortifnot(m == t(m), diag(m) == rep(1, 2)) # all TRUE
#'
#' abortifnot(1) |> try()
#'
#' # A custom error message can be given for each expression:
#' m[1, 2] <- 12
#' abortifnot("{.var m} must be {.cls symmetric}" = m == t(m)) |>
#'   try()
#'
#' # Alternatively, one error message can be used for all
#' # expressions.
#' abortifnot(
#'   m[1, 1] == 1,
#'   diag(m) == rep(2, 2),
#'   message = "{.var m} has a diagonal of: {diag(m)}"
#' ) |> try()
#'
#' # The `call` argument can be used to specify where the
#' # error occurs, by default this is the caller environment.
#' myfunc <- function(x) abortifnot(x)
#' myfunc(FALSE) |> try()
#'
#' # abortif() errors if any argument does not evaluate to
#' # (all) FALSE.
#' abortif(c(T, F)) |> try()
#' abortif(c(T, NA)) |> try()
#' @export
abortifnot <- function(
  ...,
  message = NULL,
  call = .envir,
  .envir = parent.frame(),
  .frame = .envir,
  abort_args = NULL
) {
  le <- ...length()
  if (le == 0L) {
    return(invisible(NULL))
  }

  abortifnot_impl(
    ...,
    .le = le,
    .fn = Negate(all),
    not = TRUE,
    message = message,
    call = call,
    .envir = .envir,
    .frame = .frame,
    abort_args = abort_args
  )

  invisible(NULL)
}

#' @rdname abortifnot
#' @export
abortif <- function(
  ...,
  message = NULL,
  call = .envir,
  .envir = parent.frame(),
  .frame = .envir,
  abort_args = NULL
) {
  le <- ...length()
  if (le == 0L) {
    return(invisible(NULL))
  }

  abortifnot_impl(
    ...,
    .le = le,
    .fn = any,
    not = FALSE,
    message = message,
    call = call,
    .envir = .envir,
    .frame = .frame,
    abort_args = abort_args
  )

  invisible(NULL)
}

abortifnot_impl <- function(
  ...,
  .le,
  .fn,
  not,
  message,
  call,
  .envir,
  .frame,
  abort_args
) {
  args <- enexprs(...)

  for (i in seq_len(.le)) {
    x <- ...elt(i)

    if (!is.logical(x)) {
      abort_dot_arg(
        val = x,
        arg = args[[i]],
        arg_msg = ...names()[i],
        message = message,
        problem = "type",
        call = call,
        .envir = .envir,
        .frame = .frame,
        abort_args = abort_args,
        not = not
      )
    }

    if (anyNA(x)) {
      abort_dot_arg(
        val = x,
        arg = args[[i]],
        arg_msg = ...names()[i],
        message = message,
        problem = "na",
        call = call,
        .envir = .envir,
        .frame = .frame,
        abort_args = abort_args,
        not = not
      )
    }

    if (.fn(x)) {
      abort_dot_arg(
        val = x,
        arg = args[[i]],
        arg_msg = ...names()[i],
        message = message,
        problem = "failed",
        call = call,
        .envir = .envir,
        .frame = .frame,
        abort_args = abort_args,
        not = not
      )
    }
  }
}

abort_dot_arg <- function(
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
  if (is.null(arg_msg) || !nzchar(arg_msg)) {
    arg_msg <- message %||% do_internal_msg(as_label(arg), val, problem, not)
  }

  do_abort_args_err(arg_msg, call, .envir, .frame, abort_args)
}

do_abort_args_err <- function(
  msg,
  call,
  .envir,
  .frame,
  abort_args
) {
  if ((aa_flag <- !is.null(abort_args)) && !is.list(abort_args)) {
    cli_abort(
      message = wrong_type_msg("abort_args", "a {.cls list}", abort_args),
      call = call,
      .envir = .envir,
      .frame = .frame
    )
  }

  if (aa_flag) {
    do.call(
      cli_abort,
      c(
        list(message = msg, call = call, .envir = .envir, .frame = .frame),
        abort_args
      )
    )
  }

  cli_abort(
    message = msg,
    call = call,
    .envir = .envir,
    .frame = .frame
  )
}

do_internal_msg <- function(arg, val, problem, not = TRUE) {
  switch(problem,
    type = wrong_type_msg(arg, "a {.cls logical} vector", val),
    na = na_msg(arg, length(val)),
    failed = format_inline(
      "{.arg {arg}} is {if (not) 'not ' else ''}{.val {TRUE}}."
    )
  )
}
