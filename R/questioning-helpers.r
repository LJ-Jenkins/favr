# Marked as 'questioning' - still exported and no warnings but no doc
# on website and likely to be deprecated in the future.

#-- infix

`%""%` <- function(lhs, rhs) if (lhs == "") rhs else lhs

`%!||%` <- function(lhs, rhs) if (is.null(lhs)) lhs else rhs

`%le0%` <- function(lhs, rhs) if (length(lhs) != 0) lhs else rhs

`%nm%` <- function(lhs, rhs) {
  if (is.null(names(lhs))) {
    names(lhs) <- rhs
  }
  lhs
}

#-- messaging

excl <- function(txt) {
  c("!" = txt)
}

length_or_obj <- function(x) {
  format_inline(
    if (length(x) > 1) {
      "length {.var {length(x)}}"
    } else {
      "{.var {x}}"
    }
  )
}

vec_format_inline <- function(chr, eval_env) {
  x <- c()
  for (i in seq_along(chr)) {
    x[i] <- format_inline(chr[i], .envir = eval_env)
  }
  x
}

or <- function(x) {
  cli_vec(x, style = list("vec-sep2" = " or ", "vec-last" = " or "))
}

label_btck_to_quote <- function(x) {
  gsub("\\`", "'", as_label(x))
}

full_stop <- function(x) {
  x <- as.character(x)
  if (length(x) == 0) {
    return(x)
  }
  if (!endsWith(x, ".")) {
    paste0(x, ".")
  } else {
    x
  }
}

capitalise <- function(x) {
  x <- as.character(x)
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}

as_sentence <- function(x) {
  x <- as.character(x)
  stopifnot(length(x) >= 1)
  if (length(x) == 0) {
    return(x)
  }
  x |>
    capitalise() |>
    full_stop()
}

f_list_nth_arg <- function(f, n) {
  f_all_args <- call_args(f)
  f_args <- call_args(f_all_args[[2]])
  paste(
    as_label(f_all_args[[1]]),
    "~ list(...",
    as_label(f_args[[n]]),
    "...)"
  )
}

#--

# moved from checks.R

check_logi_true <- function(logi) {
  if (length(logi) == 0) {
    abort_empty_vector()
  }

  x <- all(logi)

  if (is.na(x) || !x) {
    abort_logi_returned(
      returned = x,
      named_ele = names(logi) %!||% logi[is.na(logi) | !logi]
    )
  }
}

check_logi_false <- function(logi) {
  if (length(logi) == 0) {
    abort_empty_vector()
  }

  x <- any(logi)

  if (is.na(x) || x) {
    abort_logi_returned(
      returned = x,
      named_ele = names(logi) %!||% logi[is.na(logi) | logi]
    )
  }
}

#--

check_call_is <- function(x, fn) {
  if (!favr_env$caller_fn %in% x) {
    abort_favr(
      class = paste0(
        "favr_error_fn_called_not_in_",
        paste0(x, collapse = "_")
      ),
      caller_fn = fn,
      call = caller_env(2)
    )
  }
}

#--

check_positive_scalar_integerish <- function(
  x, arg = NULL, call = NULL, caller_fn = NULL
) {
  if (!is_scalar_integerish(x) || x <= 0) {
    abort_not_positive_scalar_integerish(
      size = x,
      arg = arg,
      call = call,
      caller_fn = caller_fn
    )
  }
}

check_env <- function(
  env, env_arg = caller_arg(env), call = NULL, caller_fn = NULL
) {
  if (!is_environment(env)) {
    abort_env(
      env_name = env_arg,
      call = call,
      caller_fn = caller_fn
    )
  }
}

check_character <- function(
  chr, chr_arg = caller_arg(chr), call = NULL, caller_fn = NULL
) {
  if (!is.character(chr)) {
    abort_not_class(
      arg = paste0("`", chr_arg, "`"),
      expected = "character",
      given = class(chr),
      call = call,
      caller_fn = caller_fn
    )
  }
}

check_args_named_and_vars_exist <- function(
  vnames,
  env,
  action,
  call = NULL,
  caller_fn = NULL
) {
  if (any(vnames == "")) {
    abort_args_unnamed(
      action = action,
      i = which(vnames == ""),
      call = call,
      caller_fn = caller_fn
    )
  }
  check_vars_exist(vnames, env, action, call, caller_fn)
}

check_vars_exist <- function(
  vnames,
  env,
  action,
  call = NULL,
  caller_fn = NULL
) {
  if (any(!vnames %in% env_names(env))) {
    abort_args_env(
      not_found = vnames[!vnames %in% env_names(env)],
      call = call,
      caller_fn = caller_fn
    )
  }
}

#--

check_size_true <- function(
  .data,
  .size,
  darg_name,
  call = NULL,
  caller_fn = NULL
) {
  if (!is.numeric(.size)) {
    abort_not_class(
      arg = "`.size`",
      expected = "numeric",
      given = class(.size),
      call = call,
      caller_fn = caller_fn
    )
  }
  check_positive_scalar_integerish(
    .size,
    arg = "`.size`",
    call = call,
    caller_fn = caller_fn
  )
  if (vec_size(.data) != .size) {
    abort_size_arg(
      arg = darg_name,
      actual_size = vec_size(.data),
      expected_size = .size,
      call = call,
      caller_fn = caller_fn
    )
  }
}

check_names_present <- function(
  .data,
  .names,
  darg_name,
  call = NULL,
  caller_fn = NULL
) {
  check_character(.names, call = call, caller_fn = caller_fn)
  if (!all(.names %in% names2(.data))) {
    abort_names_not_present(
      not_found = .names[!.names %in% names2(.data)],
      mask = darg_name,
      call = call,
      caller_fn = caller_fn
    )
  }
}

#--

# moved from args.R

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
  rhs, given, index, data, env, ...
) {
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
  rhs, given, index, data, env, ...
) {
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
