#' Ensure the truth of data-masked R expressions and cast/recycle named
#' elements.
#'
#' If any of the expressions in `...`, evaluated within the data mask
#' `'data'` (see [data masking][rlang::args_data_masking]), are not all `TRUE`, [abort][rlang::abort]
#' is called for the first which was not ([all]) `TRUE`. Alternatively,
#' [rlang formulas](https://rlang.r-lib.org/reference/new_formula.html) can
#' be used to take advantage of [tidyselect](https://tidyselect.r-lib.org/index.html)
#' features and pass multiple named elements in `data` to validation
#' formulas/functions, and/or attempt safe type casting and size recycling
#' using the [favr::cast], [favr::recycle] and [favr::coerce] functions.
#' The rhs of formulas can be given in a [list] to pass multiple
#' functions/formulas/calls. The `.names` and `.size` arguments can also be used
#' to check for given names and size of the data.frame/list itself. Type
#' casting, size checking, and recycling are undertaken using the
#' [vctrs](https://vctrs.r-lib.org/) package and thus apply
#' [vctrs type and size rules](https://vctrs.r-lib.org/articles/type-size.html).
#'
#' @param data a data.frame or list to use as the data mask.
#' @param ... any number of R expressions or formulas to be evaluated using
#' `data` as a data mask. Formulas can use tidyselect syntax
#' on the lhs and either functions or formulas that evaluate to logical, or one
#' of the type/size functions: [favr::cast], [favr::recycle] and [favr::coerce]
#' on the rhs. The rhs of a formula can also be a [list] of multiple
#' functions/formulas/calls. If an expression is named, or if the list
#' element on the rhs of a formula is named, the name is passed to
#' [format_inline][cli::format_inline] and is used in the error message.
#' @param .names character vector of names which must be present in the `data`
#' data.frame/list.
#' @param .size positive scalar integerish value for the size that the `data`
#' data.frame/list must be.
#' @param .error_call the call environment to use for error messages
#' (passed to [abort][rlang::abort]).
#' @param .darg the argument name of `data` to use in error messages.
#' @return `data` is returned with attached class `with_schema` and
#' attribute `schema` containing the schema call to be enforced later.
#' @details See [favr::abort_if_not] for a non-data-masked validation tool and
#' [favr::enforce] for a non-data-masked version of this function.
#' @examples
#' # NB: Some of these examples are expected to produce an error. To
#' #     prevent them from terminating a run with example() they are
#' #     piped into a call to try().
#'
#' li <- list(x = 1L, y = "hi", z = \(x) x > 1)
#' li <- li |>
#'   schema(x == 1, is.character(y), is.function(z)) # all TRUE
#'
#' # The schema call is attached to the returned object and
#' # can be re-evaluated using enforce_schema():
#' li <- enforce_schema(li) # no error
#' li2 <- li
#' li2$x <- 2L
#' enforce_schema(li2) |> try()
#' # Error:
#' # Caused by error in `enforce_schema()`:
#' # ℹ In argument: `x == 1`.
#' # ! Returned `FALSE`.
#'
#' # Calling `schema()` again overwrites any existing schema.
#' # Alternatively use `add_to_schema()` to add arguments to
#' # an existing schema (.size overwrites, other args append):
#' li <- li |>
#'   add_to_schema(is.numeric(x), .names = c("x", "y"), .size = 3)
#'
#' # A custom error message can be given for each expression by
#' # naming it:
#' schema(li,
#'   "{.var y} must be {.cls numeric}, check input" = is.numeric(y)
#' ) |> try()
#' # Error:
#' # Caused by error in `schema()`:
#' # ℹ In argument: `is.numeric(y)`.
#' # ! `y` must be <numeric>, check input
#'
#' # Formulas can be used to take advantage of tidyselect features
#' # on the lhs, with functions/additional formulas required on
#' # the rhs:
#' schema(li,
#'   "multiple columns: {.pkg tidyselect}" = c(x, y) ~ is.integer
#' ) |> try()
#' # Error:
#' # Caused by error in `schema()`:
#' # ℹ For named element: `y`.
#' # ℹ In argument: `c(x, y) ~ is.integer`.
#' # ! multiple columns: tidyselect
#'
#' # Formulas can also be used with `cast()`, `recycle()`, and
#' # `coerce()` on the rhs to safely cast or recycle named
#' # elements:
#' class(schema(li, x ~ cast(double()))$x) # x is now numeric
#' length(schema(li, x ~ recycle(5))$x) # x is now length 5
#' schema(
#'   li,
#'   y ~ coerce(type = factor(), size = 5)
#' )$y # y is now factor and length 5
#'
#' # Multiple calls can be used with formulas by wrapping them
#' # in `list()`, with the names of list elements being
#' # preferentially chosen for error messaging and the error
#' # message also showing which formula/function/call caused the
#' # error:
#' schema(
#'   li,
#'   "generic message" = c(x, y, z) ~ list(
#'     Negate(is.null),
#'     "{.var specific} message" = Negate(is.function)
#'   )
#' ) |> try()
#' # Error:
#' # Caused by error in `schema()`:
#' # ℹ For named element: `z`.
#' # ℹ In argument: `c(x, y, z) ~ list(... Negate(is.function) ...)`.
#' # ! `specific` message
#'
#' # Changed elements are available immediately:
#' df <- data.frame(x = 1L, y = 1L)
#' lapply(schema(df, x ~ cast(double()), y ~ cast(x)), class)
#' # both now "numeric"
#'
#' # `.names` and `.size` arguments can be used to check that given
#' # names are present and that the data has the desired size:
#' schema(li, .names = c("a", "x", "y", "b")) |> try()
#' # Error:
#' # Caused by error in `schema()`.
#' # ! Named elements `a` and `b` not found in data mask `li`.
#'
#' schema(li, .size = 5) |> try()
#' # Error:
#' # Caused by error in `schema()`.
#' # ! Object `li` is of vctrs size `3`, not `5`.
#'
#' # The `.error_call` argument can be used to specify where the
#' # error occurs, by default this is the caller environment:
#' myfunc <- function(x, ...) schema(x, ...)
#' myfunc(li, x > 4) |> try()
#' # Error in `myfunc()`:
#' # Caused by error in `schema()`:
#' # ℹ In argument: `x > 4`.
#' # ! Returned `FALSE`.
#'
#' # rlang pronouns and injection can be used, but care must be
#' # taken when using `.env` and `enforce_schema()` as the
#' # caller environment may have changed:
#' msg <- "{.var injection} msg"
#' cols <- quote(c(x, y))
#' schema(li, !!msg := !!cols ~ is.integer) |> try()
#' # Error:
#' # Caused by error in `schema()`:
#' # ℹ For named element: `y`.
#' # ℹ In argument: `c(x, y) ~ is.integer`.
#' # ! `injection` msg
#'
#' x <- 1L
#' li <- schema(li, x == .env$x) # no error
#' x <- 2
#' enforce_schema(li) |>
#'   try() # error as the environmental variable has changed
#' # Error:
#' # Caused by error in `enforce_schema()`:
#' # ℹ In argument: `x == .env$x`.
#' # ! Returned `FALSE`.
#' @export
schema <- function(data, ...) {
  UseMethod("schema", data)
}

#' @export
schema.default <- function(
    data,
    ...,
    .error_call = caller_env(),
    .darg = caller_arg(data)) {
  schema_default_dispatch(
    data,
    ...,
    .error_call = .error_call,
    .darg = .darg,
    .caller_fn = "schema"
  )
}

#' @rdname schema
#' @export
schema.list <- function(
    data,
    ...,
    .names = NULL,
    .size = NULL,
    .error_call = caller_env(),
    .darg = caller_arg(data)) {
  schema_dispatch(
    data,
    ...,
    .names = .names,
    .size = .size,
    .error_call = .error_call,
    .darg = .darg,
  )
}

#' @rdname schema
#' @export
schema.data.frame <- function(
    data,
    ...,
    .names = NULL,
    .size = NULL,
    .error_call = caller_env(),
    .darg = caller_arg(data)) {
  schema_dispatch(
    data,
    ...,
    .names = .names,
    .size = .size,
    .error_call = .error_call,
    .darg = .darg,
  )
}

schema_dispatch <- function(
    data,
    ...,
    .names,
    .size,
    .error_call,
    .darg,
    .caller_fn = "schema") {
  args <- enquos(...)
  eval_env <- caller_env(2)
  check_env(.error_call, call = eval_env, caller_fn = .caller_fn)
  check_character(.darg, call = .error_call, caller_fn = .caller_fn)

  error_names <- vec_format_inline(
    names2(args),
    eval_env = eval_env
  )

  eval_masked_exprs(
    data,
    args,
    error_names,
    eval_env,
    .names = .names,
    .size = .size,
    .error_call = .error_call,
    .darg = .darg,
    .caller_fn = .caller_fn
  )
}

eval_masked_exprs <- function(
    data,
    args,
    error_names,
    eval_env,
    .names,
    .size,
    .error_call,
    .darg,
    .caller_fn,
    enforcing = FALSE) {
  if (!is.null(.size)) {
    check_size_true(
      data,
      .size,
      .darg,
      call = .error_call,
      caller_fn = .caller_fn
    )
  }

  if (!is.null(.names)) {
    check_names_present(
      data,
      .names,
      .darg,
      call = .error_call,
      caller_fn = .caller_fn
    )
  }

  if (length(args) == 0) {
    if (!enforcing) {
      data <- attach_schema(
        data,
        .class = "favr:::schema",
        error_names = error_names,
        mask_names = .names,
        mask_size = .size
      )
    }

    return(data)
  }

  add_caller_fn("schema")

  withCallingHandlers(
    for (i in seq_along(args)) {
      given <- NULL # stop previous value leaking into error message

      arg <- eval_tidy(args[[i]], data = data, env = eval_env)
      given <- check_logi_or_return_formula(arg)

      if (!is.null(given)) {
        given <- tidy_formula_split_eval(given, data, eval_env)

        # loop and keep indexes visible to handler
        for (k in seq_along(given$rhs)) {
          for (j in seq_along(given$pos)) {
            data <- eval_schema_formula_rhs(
              prep_formula_rhs(given$rhs[[k]]),
              given,
              j,
              data,
              eval_env
            )
          }
        }
      }
    },
    error = function(cnd) {
      clean_favr_env_on_exit()

      favr_error_handler(
        cnd = cnd,
        caller_fn = .caller_fn,
        expr = if (length(given$rhs) > 1) {
          f_list_nth_arg(args[[i]], k)
        } else {
          label_btck_to_quote(args[[i]])
        },
        arg = given$vars %!||% given$vars[j],
        msg = if (!is.null(given$rhs)) {
          format_inline(
            names2(given$rhs[k]),
            .envir = eval_env
          ) %""% error_names[i] %""% NULL
        } else {
          error_names[i] %""% NULL
        },
        call = .error_call
      )
    }
  )
  if (!enforcing) {
    data <- attach_schema(
      data,
      .class = "favr:::schema",
      args = args,
      error_names = error_names,
      mask_names = .names,
      mask_size = .size
    )
  }

  clean_favr_env_on_exit()
  return(data)
}

#--

#' @export
print.with_schema <- function(x, ...) {
  attr(x, "schema") <- NULL
  NextMethod()
}

attach_schema <- function(.data, .class, ...) {
  attr(.data, "schema") <- structure(
    list(...),
    class = .class
  )

  if (!inherits(.data, "with_schema")) {
    class(.data) <- c("with_schema", class(.data))
  }

  .data
}

#--

#' @rdname schema
#' @export
enforce_schema <- function(data, ...) {
  UseMethod("enforce_schema", data)
}

#' @export
enforce_schema.default <- function(
    data,
    ...,
    .error_call = caller_env(),
    .darg = caller_arg(data)) {
  schema_default_dispatch(
    data,
    ...,
    .error_call = .error_call,
    .darg = .darg,
    .caller_fn = "enforce_schema"
  )
}

#' @rdname schema
#' @export
enforce_schema.with_schema <- function(
    data,
    ...,
    .error_call = caller_env(),
    .darg = caller_arg(data)) {
  check_dots_empty0(...)
  attached_schema <- attr(data, "schema")
  eval_masked_exprs(
    data,
    attached_schema$args,
    attached_schema$error_names,
    caller_env(),
    attached_schema$mask_names,
    attached_schema$mask_size,
    .error_call,
    .darg,
    .caller_fn = "enforce_schema",
    enforcing = TRUE
  )
}

#--

#' @rdname schema
#' @export
add_to_schema <- function(data, ...) {
  UseMethod("add_to_schema", data)
}

#' @export
add_to_schema.default <- function(
    data,
    ...,
    .error_call = caller_env(),
    .darg = caller_arg(data)) {
  schema_default_dispatch(
    data,
    ...,
    .error_call = .error_call,
    .darg = .darg,
    .caller_fn = "add_to_schema"
  )
}

#' @rdname schema
#' @export
add_to_schema.with_schema <- function(
    data,
    ...,
    .names = NULL,
    .size = NULL,
    .error_call = caller_env(),
    .darg = caller_arg(data)) {
  args <- enquos(...)
  eval_env <- caller_env()
  caller_fn <- "add_to_schema"
  check_env(.error_call, call = eval_env, caller_fn = caller_fn)
  check_character(.darg, call = .error_call, caller_fn = caller_fn)

  attached_schema <- attr(data, "schema")

  # append args
  attached_schema$args <- c(attached_schema$args, args)

  # append errors msgs
  attached_schema$error_names <- c(
    attached_schema$error_names,
    vec_format_inline(
      names2(args),
      eval_env = eval_env
    )
  )

  # append names if given
  if (!is.null(.names)) {
    attached_schema$mask_names <- c(
      attached_schema$mask_names, .names
    )
  }

  # overwrite size if given
  attached_schema$mask_size <- .size %||% attached_schema$mask_size

  eval_masked_exprs(
    data,
    attached_schema$args,
    attached_schema$error_names,
    eval_env,
    attached_schema$mask_names,
    attached_schema$mask_size,
    .error_call,
    .darg,
    .caller_fn = caller_fn
  )
}

#--

schema_default_dispatch <- function(
    data,
    ...,
    .error_call,
    .darg,
    .caller_fn) {
  schema <- NULL
  if (.caller_fn %in% c("enforce_schema", "add_to_schema")) {
    schema <- " with an attached schema"
  }
  check_env(.error_call, call = caller_env(2), caller_fn = .caller_fn)
  check_character(.darg, call = .error_call, caller_fn = .caller_fn)
  abort_s3_not_applicable(
    darg = .darg,
    expected_class = "data.frame/list",
    schema = schema,
    caller_fn = .caller_fn,
    call = .error_call
  )
}

#--
