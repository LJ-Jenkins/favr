#' Cast objects to a given type
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
#' (passed to [rlang::abort]).
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
#' class(x) # "numeric"
#'
#' # By default, lossy casting is not allowed:
#' x <- c(1, 1.5)
#' cast_if_not(x = integer()) |> try()
#' # Error:
#' # Caused by error in `cast_if_not()`:
#' # ℹ In argument: `x = integer()`.
#' # ! Can't convert from `x` <double> to <integer> due to loss of precision.
#' # ✖ • Locations: 2
#'
#' # lossy casting can be enabled using `lossy()` call:
#' cast_if_not(x = lossy(integer()))
#' class(x)
#' # "integer"
#'
#' # Other objects can be used as the type to cast to, e.g.:
#' x <- 1L
#' y <- 2.3
#' cast_if_not(x = y)
#' class(x)
#' # "numeric"
#'
#' # Changed objects are available immediately:
#' x <- y <- 1L
#' cast_if_not(x = double(), y = x)
#' cat(class(x), class(y), sep = ", ")
#' # numeric, numeric
#'
#' myfunc <- \(x) {
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
#' )
#' # environment 'e', numeric, local environment, integer
#'
#' # Named objects (lhs) are checked to be in the `.env` environment,
#' # throwing an error if not found:
#' x <- 1L
#' e <- new.env()
#' cast_if_not(x = 1.5, .env = e) |> try()
#' # Error:
#' # Caused by error in `cast_if_not()`.
#' # ! Object `x` is not found in the `.env` environment specified.
#'
#' # For expressions (rhs), the `.env` argument is preferentially
#' # chosen, but if not found then the normal R scoping rules
#' # apply:
#' x <- 1.5
#' e <- new.env()
#' e$z <- 1L
#' cast_if_not(z = x, .env = e)
#' class(e$z)
#' # "numeric"
#'
#' # The `.error_call` argument can be used to specify where the
#' # error occurs, by default this is the caller environment:
#' myfunc <- function(x) cast_if_not(x = character())
#' myfunc(FALSE) |> try()
#' # Error in `myfunc()`:
#' # Caused by error in `cast_if_not()`:
#' # ℹ In argument: `x = character()`.
#' # ! Can't convert `x` <logical> to <character>.
#'
#' # Injection can be used:
#' y <- 1L
#' x <- "y"
#' cast_if_not(!!x := double()) |> try()
#' class(y)
#' # "numeric"
#' y <- 1L
#' x <- list(y = double())
#' cast_if_not(!!!x)
#' class(y)
#' # "numeric"
#'
#' # Objects are reverted to their original values if an error
#' # occur:
#' x <- y <- 1L
#' cast_if_not(x = double(), y = character()) |> try() # errors
#' class(x) # integer
#' @export
cast_if_not <- function(
    ...,
    .env = caller_env(),
    .error_call = caller_env()) {
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

#--
