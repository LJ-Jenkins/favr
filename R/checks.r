check_logi_true <- function(logi) {
  if (length(logi) == 0) {
    abort_empty_vector()
  }

  x <- all(logi)

  if (is.na(x) || !x) {
    abort_logi_returned(returned = x)
  }
}

check_logi_false <- function(logi) {
  if (length(logi) == 0) {
    abort_empty_vector()
  }

  x <- any(logi)

  if (is.na(x) || x) {
    abort_logi_returned(returned = x)
  }
}

#--

check_call_is <- function(x, fn = NULL) {
  if (!favr_env$caller_fn %in% x) {
    abort_favr(
      class = paste0(
        "favr_error_fn_called_not_in_",
        paste0(x, collapse = "_")
      ),
      fn = fn
    )
  }
}

#--

check_positive_scalar_integerish <- function(
    x, arg = NULL, call = NULL, caller_fn = NULL) {
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
    env, env_arg = caller_arg(env), call = NULL, caller_fn = NULL) {
  if (!is_environment(env)) {
    abort_env(
      env_name = env_arg,
      call = call,
      caller_fn = caller_fn
    )
  }
}

check_character <- function(
    chr, chr_arg = caller_arg(chr), call = NULL, caller_fn = NULL) {
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
    caller_fn = NULL) {
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
    caller_fn = NULL) {
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
    caller_fn = NULL) {
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
    caller_fn = NULL) {
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
