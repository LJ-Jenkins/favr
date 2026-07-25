#' Modify behaviour of check functions
#'
#' @description
#' Modify the type-checking, or length-checking behaviour of [favr]
#' type checking functions.
#' @param x An object to check if bare.
#' @param n,n_min,n_max Single numeric value that is castable to an integer.
#' Must be zero or positive.
#' @param arg,arg_min,arg_max An argument name as a string. This argument will
#' be mentioned in error messages as the input that is at the origin of a
#' problem.
#' @return A list of class `favr_modifier` with named elements `obj`, `bare`
#' and `arg` for `bare()`, and `at_least` and/or `at_most` for the length
#' modifiers.
#' @details
#' Use `bare()` to check if a given object is a bare R object
#' (no class attribute), throwing an error if it is not and passing the
#' object on to the check if it is.
#'
#' For S3 type checks, `bare()` checks that the object has the expected S3
#' type as the **first** element of the class vector.
#'
#' To modify the behaviour of the length checking `n` argument:
#'
#' * `at_least(n)` means the object must be at least length (`>=`) `n`.
#' * `at_most(n)` means the object must be at most length (`<=`) `n`.
#' * `in_range(n_min, n_max)` means the object length must be within the range
#' of (`>=`) `n_min` and (`<=`) `n_max`.
#' @note `bare()` is a wrapper of [is.object] and cannot be used with the
#' [scalar-value-checks].
#' @name modifiers
#' @seealso [type-checks] and [scalar-type-checks] for the functions that
#' these modifiers can be used with.
#' @examples
#' bare(1)
#' at_least(1)
#' at_most(1)
#' in_range(1, 2)
#'
#' at_least(1.5) |> try()
#'
#' check_integer(bare(factor(1))) |> try()
#' check_integer(1:5, n = at_least(10)) |> try()
#' check_integer(1:5, n = at_most(3)) |> try()
#' check_integer(1:5, n = in_range(2, 4)) |> try()
#'
#' x <- as.Date("2000-01-01")
#' class(x) <- c("my_date", class(x))
#' check_date(bare(x)) |> try()
NULL

# to do:
# a vec_size() variant?

#' @rdname modifiers
#' @export
bare <- function(x, arg = caller_arg(x)) {
  setClass(
    list(obj = x, bare = !is.object(x), arg = arg),
    c("favr_bare", "favr_modifier")
  )
}

#' @rdname modifiers
#' @export
at_least <- function(n, arg = caller_arg(n)) {
  n <- list(
    at_least = modifier_cast_integer(n, arg, "at_least 'n'")
  )

  setClass(n, c("favr_at_least", "favr_modifier"))
}

#' @rdname modifiers
#' @export
at_most <- function(n, arg = caller_arg(n)) {
  n <- list(
    at_most = modifier_cast_integer(n, arg, "at_most 'n'")
  )

  setClass(n, c("favr_at_most", "favr_modifier"))
}

#' @rdname modifiers
#' @export
in_range <- function(
  n_min,
  n_max,
  arg_min = caller_arg(n_min),
  arg_max = caller_arg(n_max)
) {
  n <- c(n_min, n_max)
  arg <- paste0("c(", arg_min, ", ", arg_max, ")")
  n <- modifier_cast_integer(n, arg, "in range 'n'", in_range = TRUE)

  n <- list(
    at_least = n[[1]],
    at_most = n[[2]]
  )

  if (n[[1]] > n[[2]]) {
    cli_abort("{.arg {arg}} must be a valid range in the form of {.arg c(n_min, n_max)}, but {.var n[1]} {.val {n[[1]]}} is greater than {.var n[2]} {.val {n[[2]]}}.")
  }

  setClass(
    list(at_least = n[[1]], at_most = n[[2]]),
    c("favr_in_range", "favr_modifier")
  )
}

setClass <- function(x, class) {
  class(x) <- class
  x
}

modifier_cast_integer <- function(x, x_arg, to_arg, in_range = FALSE) {
  call <- caller_env()
  x <- vctrs::vec_cast(
    x, integer(),
    x_arg = x_arg, to_arg = to_arg, call = call
  )

  if (in_range) {
    check_n_min_n_max(
      n = x,
      call = call
    )
  } else {
    check_n(
      n = x,
      n_arg = x_arg,
      call = call
    )
  }

  x
}

check_n <- function(n, n_arg, call) {
  if (length(n) != 1L) {
    cli_abort(
      "{.arg {n_arg}} must be a single numeric value, but is of length {.val {length(n)}}.",
      call = call
    )
  }

  if (n < 0) {
    cli_abort(
      "{.arg {n_arg}} must be greater than or equal to zero, but {.val {n}} was provided.",
      call = call
    )
  }
}

check_n_min_n_max <- function(n, call) {
  if (length(n) != 2L) {
    cli_abort(
      "{.arg n_min} and {.arg n_max} must be single numeric values, but combined are of length {.val {length(n)}}.",
      call = call
    )
  }

  check_n(n[[1]], "n_min", call)
  check_n(n[[2]], "n_max", call)
}
