#' Apply a predicate check to each element of a vector
#'
#' @description
#' Apply a predicate check function to each element of a vector and
#' throw an error if any element fails the check.
#' @param .x A list or atomic vector.
#' @param .f A function or formula to apply to each element of `.x`.
#' Must return a logical vector of [all] `TRUE` for no error to occur.
#' Non-`logical` and `NA` values will trigger an error.
#' @param ... Additional arguments passed to [cli_abort()][cli::cli_abort]
#' which forwards unmatched arguments to [abort()][rlang::abort].
#' @param call
#' The execution environment of a currently running function,
#' e.g. `caller_env()`. The function will be mentioned in error
#' messages as the source of the error. See the call argument of
#' [abort()][rlang::abort] for more information.
#' @return
#' `.x` invisibly if all checks pass, otherwise
#' an error is thrown.
#' @details
#' If you wish to use a function for `.f` that errors,
#' pass contextual information to that function
#' directly (e.g., using a shorthand anonymous function),
#' as the error will be thrown from that function's context
#' and won't have access to `...` and `call` from the
#' calling function.
#' @name walk-check
#' @family checks
#' @examples
#' x <- list(1, 2, "a")
#' walk_check(x, is.atomic)
#' walk_check(x, ~ length(.x) == 1L)
#' walk_check(x, is.numeric) |> try()
#' walk_check(x, \(el) nchar(el) == 1L)
#'
#' # Named elements are shown in the error.
#' x <- list(a = 1, b = 2, c = "a")
#' walk_check(x, is.numeric) |> try()
NULL

#' @rdname walk-check
#' @export
walk_check <- function(.x, .f, ..., call = caller_env()) {
  if (length(.x) == 0L) {
    return(.x)
  }

  if (!is_vector(.x)) {
    cli_abort(
      message = wrong_type_msg(".x", "a {.cls vector}", .x),
      ...,
      call = call
    )
  }

  .f <- as_function(.f)

  walk_check_impl(.x, .f, ..., call = call)
}

walk_check_impl <- function(.x, .f, ..., call) {
  for (i in seq_along(.x)) {
    res <- .f(.x[[i]])

    if (!is.logical(res)) {
      abort_element(
        el_pos = i,
        el_name = names(.x)[i],
        problem = paste0(
          "must be a {.cls logical} vector, not ",
          type_friendly(res, value = TRUE, length = length(res))
        ),
        ...,
        call = call
      )
    }

    if (anyNA(res)) {
      abort_element(
        el_pos = i,
        el_name = names(.x)[i],
        problem = if (length(res) == 1L) {
          "is {.val {NA}}"
        } else {
          "contains {.val {NA}} values"
        },
        ...,
        call = call
      )
    }

    if (!all(res)) {
      abort_element(
        el_pos = i,
        el_name = names(.x)[i],
        problem = "is not {.val {TRUE}}",
        ...,
        call = call
      )
    }
  }

  invisible(.x)
}

abort_element <- function(
  el_pos,
  el_name,
  problem,
  ...,
  call
) {
  loc <- if (is.null(el_name) || !nzchar(el_name)) {
    paste0("{.val {", el_pos, "}}]]}")
  } else {
    paste0(col_blue("'", el_name, "'"), "]]} (index: {.val {el_pos}})")
  }

  # format early in case of `.envir` given in dots.
  msg <- format_inline("Check result for {.arg .x[[", loc, " ", problem, ".")
  cli_abort(message = msg, ..., call = call)
}
