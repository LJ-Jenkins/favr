#--

check_logi_or_return_formula <- function(arg) {
  if (!is.logical(arg) && !is_formula(arg)) {
    abort_not_class(
      expected = c("logical", "formula"),
      given = class(arg)
    )
  } else if (is.logical(arg)) {
    check_logi_true(arg)
    NULL
  } else if (is_formula(arg)) {
    arg
  }
}

f_lhs_tidyselect <- function(f, data) {
  eval_select(f_lhs(f), data, allow_empty = FALSE)
}

f_rhs_eval_as_list <- function(f, data, env) {
  x <- eval_tidy(f_rhs(f), data = data, env = env)
  if (!is.list(x) || inherits(x, "favr:::arg")) list(x) else x
}

formula_split_eval_rhs <- function(f, env) {
  lhs <- f_lhs(f)
  if (is_call(lhs)) {
    if (call_name(lhs) != "c") {
      abort_favr(class = "favr_error_not_c")
    }
    lhs <- call_args(lhs)
  }
  if (!is.list(lhs)) {
    lhs <- list(lhs)
  }

  rhs <- f_rhs_eval_as_list(f, NULL, env)

  list(lhs = lhs, rhs = rhs)
}

tidy_formula_split_eval <- function(formula, data, env) {
  lhs_pos <- f_lhs_tidyselect(formula, data)
  lhs_vars <- names(lhs_pos)
  rhs <- f_rhs_eval_as_list(formula, data, env)

  list(pos = lhs_pos, vars = lhs_vars, rhs = rhs)
}

prep_formula_rhs <- function(rhs) {
  if (is_formula(rhs)) {
    rhs <- as_function(rhs)
  }

  if (!is.function(rhs) && !inherits(rhs, "favr:::arg")) {
    abort_rhs_formula(given = class(rhs))
  } else {
    rhs
  }
}

#--

eval_formula_rhs <- function(rhs, lhs, env, ...) {
  UseMethod("eval_formula_rhs", rhs)
}

#' @export
eval_formula_rhs.function <- function(rhs, lhs, env, ...) {
  eval_call <- call2(rhs, lhs)
  logi <- eval_tidy(eval_call, env = env)

  if (!is.logical(logi)) {
    abort_not_class(expected = "logical", given = class(logi))
  }
  check_logi_true(logi)

  NULL
}

#' @export
`eval_formula_rhs.favr:::arg` <- function(rhs, lhs, env, ...) {
  var <- as.character(lhs)
  if (!is.null(attr(rhs, "arg_cast"))) {
    env[[var]] <- vec_cast_lossy(
      env[[var]],
      rhs$type,
      var,
      allow_lossy = rhs$lossy
    )
  }

  if (!is.null(attr(rhs, "arg_recycle"))) {
    env[[var]] <- vec_recycle(
      env[[var]],
      rhs$size,
      x_arg = var
    )
  }

  NULL
}

eval_schema_formula_rhs <- function(rhs, given, index, data, env, ...) {
  UseMethod("eval_schema_formula_rhs", rhs)
}

#' @export
eval_schema_formula_rhs.function <- function(
    rhs, given, index, data, env, ...) {
  eval_call <- call2(rhs, data[[given$pos[index]]])
  logi <- eval_tidy(eval_call, data = data, env = env)

  if (!is.logical(logi)) {
    abort_not_class(expected = "logical", given = class(logi))
  }
  check_logi_true(logi)

  data
}

#' @export
`eval_schema_formula_rhs.favr:::arg` <- function(
    rhs, given, index, data, env, ...) {
  if (!is.null(attr(rhs, "arg_cast"))) {
    data[[given$pos[index]]] <- vec_cast_lossy(
      data[[given$pos[index]]],
      rhs$type,
      given$vars[index],
      allow_lossy = rhs$lossy
    )
  }

  if (!is.null(attr(rhs, "arg_recycle"))) {
    if (is.data.frame(data)) {
      abort_recycle_not_list()
    }

    data[[given$pos[index]]] <- vec_recycle(
      data[[given$pos[index]]],
      rhs$size,
      x_arg = given$vars[index]
    )
  }

  data
}

#--

# schema flow of expressions and accepted types

# expression
# ├─ list (og logi or formula)
# ├─ logical
# └─ formula
#    ├─ formula
#    │  └─ function
#    │     └─ logical
#    ├─ function
#    │  └─ logical
#    └─ call
#       ├─ cast
#       │  └─ anything
#       ├─ lossy
#       │  └─ anything (unchecked, but should be boolean)
#       ├─ recycle
#       │  └─ scalar integerish
#       └─ coerce
#          ├─ cast
#          │  └─ anything
#          ├─ recycle
#          │  └─ scalar integerish
#          └─ lossy
#             └─ anything (unchecked, but should be boolean)

# recycle only implemented for lists
