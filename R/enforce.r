#' Ensure the truth of R expressions and cast/recycle objects.
#'
#' If any of the expressions in `...` are not all `TRUE`, [abort][rlang::abort]
#' is called for the first which was not ([all]) `TRUE`. Alternatively,
#' [rlang formulas](https://rlang.r-lib.org/reference/new_formula.html) can
#' be used to pass multiple objects to validation formulas/functions,
#' and/or attempt safe type casting and size recycling  using the
#' [favr::cast], [favr::recycle] and [favr::coerce] functions. The rhs of
#' formulas can be given in a [list] to pass multiple functions/formulas/calls.
#' Expressions are evaluated in the environment specified and objects
#' are assigned back into that environment. Type casting and recycling
#' are undertaken using the [vctrs](https://vctrs.r-lib.org/) package
#' and thus apply [vctrs type and size rules](https://vctrs.r-lib.org/articles/type-size.html).
#'
#' @param ... any number of R expressions or formulas to be evaluated.
#' Expressions must evaluate to logical whilst formulas can use [c] on
#' the lhs and either functions or formulas that evaluate to logical,
#' or one of the type/size functions: [favr::cast], [favr::recycle] or
#' [favr::coerce] on the rhs. The rhs of a formula can also be a [list]
#' of multiple functions/formulas/calls. If an expression is named, or
#' if the list element on the rhs of a formula is named, the name is passed to
#' [format_inline][cli::format_inline] and is used in the error message.
#' @param .env the environment to use for the evaluation of the
#' expressions and the assignment of the objects.
#' @param .error_call the call environment to use for error messages
#' (passed to [abort][rlang::abort]).
#' @return NULL, but objects casted/recycled in `...` will be changed in
#' the `.env` environment specified.
#' @details See [favr::abort_if_not] for only validations and
#' [favr::schema] for a data-masked version of this function.
#' @examples
#' # NB: Some of these examples are expected to produce an error. To
#' #     prevent them from terminating a run with example() they are
#' #     piped into a call to try().
#'
#' x <- 1L
#' y <- "hi"
#' z <- \(x) x > 1
#' enforce(x == 1, is.character(y), is.function(z)) # all TRUE
#'
#' enforce(x == 2) |> try()
#' # Error:
#' # Caused by error in `enforce()`:
#' # ℹ In argument: `x == 2`.
#' # ! Returned `FALSE`.
#'
#' # A custom error message can be given for each expression by
#' # naming it:
#' enforce(
#'   "{.var y} must be {.cls numeric}, check input" = is.numeric(y)
#' ) |> try()
#' # Error:
#' # Caused by error in `enforce()`:
#' # ℹ In argument: `is.numeric(y)`.
#' # ! `y` must be <numeric>, check input
#'
#' # Formulas can be used to take pass multiple objects
#' # on the lhs, with functions/additional formulas required on
#' # the rhs:
#' enforce(
#'   "multiple objects using: {.fn c}" = c(x, y) ~ is.integer
#' ) |> try()
#' # Error:
#' # Caused by error in `enforce()`:
#' # ℹ For named element: `y`.
#' # ℹ In argument: `c(x, y) ~ is.integer`.
#' # ! multiple objects using: `c()`
#'
#' # Formulas can also be used with `cast()`, `recycle()`, and
#' # `coerce()` on the rhs to safely cast or recycle objects:
#' enforce(x ~ cast(double()))
#' class(x) # x is now numeric
#' enforce(x ~ recycle(5))
#' length(x) # x is now length 5
#' enforce(y ~ coerce(type = factor(), size = 5))
#' print(y) # y is now factor and length 5
#'
#' # Multiple calls can be used with formulas by wrapping them
#' # in `list()`, with the names of list elements being
#' # preferentially chosen for error messaging and the error
#' # message also showing which formula/function/call caused the
#' # error:
#' enforce(
#'   "generic message" = c(x, y, z) ~ list(
#'     Negate(is.null),
#'     "{.var specific} message" = Negate(is.function)
#'   )
#' ) |> try()
#' # Error:
#' # Caused by error in `enforce()`:
#' # ℹ For named element: `z`.
#' # ℹ In argument: `c(x, y, z) ~ list(... Negate(is.function) ...)`.
#' # ! `specific` message
#'
#' # Changed elements are available immediately:
#' x <- y <- 1L
#' enforce(x ~ cast(double()), y ~ cast(x))
#' cat(class(x), class(y)) # both now "numeric"
#'
#' # The `.error_call` argument can be used to specify where the
#' # error occurs, by default this is the caller environment:
#' myfunc <- function(...) enforce(...)
#' myfunc(x > 4) |> try()
#' # Error in `myfunc()`:
#' # Caused by error in `enforce()`.
#' # ℹ In argument: `x > 4`.
#' # ! Returned `FALSE`.
#'
#' # rlang injection can be used:
#' msg <- "{.var injection} msg"
#' cols <- quote(c(x, y))
#' enforce(!!msg := !!cols ~ is.integer) |> try()
#' # Error:
#' # Caused by error in `enforce()`:
#' # ℹ For named element: `x`.
#' # ℹ In argument: `c(x, y) ~ is.integer`.
#' # ! `injection` msg
#'
#' # Objects are reverted to their original values if an error
#' # occur:
#' x <- y <- 1L
#' enforce(
#'   x ~ cast(double()), y ~ recycle(5), y ~ is.function
#' ) |> try() # errors
#' class(x) # integer
#' length(y) # 1
#' @export
enforce <- function(
    ...,
    .env = caller_env(),
    .error_call = caller_env()) {
  args <- enquos(...)
  caller_fn <- "enforce"

  check_env(.error_call, call = caller_env(), caller_fn = caller_fn)
  check_env(.env, call = .error_call, caller_fn = caller_fn)

  error_names <- vec_format_inline(
    names2(args),
    eval_env = .env
  )

  add_caller_fn(caller_fn)

  withCallingHandlers(
    for (i in seq_along(args)) {
      given <- NULL # stop previous value leaking into error message

      arg <- eval_tidy(args[[i]], env = .env)
      given <- check_logi_or_return_formula(arg)

      if (!is.null(given)) {
        given <- formula_split_eval_rhs(given, .env)

        # loop and keep indexes visible to handler
        for (k in seq_along(given$rhs)) {
          given_vars_chr <- as.character(given$lhs)

          check_vars_exist(given_vars_chr, .env, "enforce")

          for (j in seq_along(given$lhs)) {
            collect_old_value(given_vars_chr[j], .env)

            eval_formula_rhs(
              prep_formula_rhs(given$rhs[[k]]),
              given$lhs[[j]],
              .env
            )
          }
        }
      }
    },
    error = function(cnd) {
      restore_old_values(.env)

      favr_error_handler(
        cnd = cnd,
        caller_fn = caller_fn,
        expr = if (length(given$rhs) > 1) {
          f_list_nth_arg(args[[i]], k)
        } else {
          label_btck_to_quote(args[[i]])
        },
        arg = if (!is.null(given$lhs) && length(given$lhs) > 1) given$lhs[[j]],
        msg = if (!is.null(given$rhs)) {
          format_inline(
            names2(given$rhs[k]),
            .envir = .env
          ) %""% error_names[i] %""% NULL
        } else {
          error_names[i] %""% NULL
        },
        call = .error_call
      )
    }
  )

  clean_favr_env_on_exit()
}

#--
