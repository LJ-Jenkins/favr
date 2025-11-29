# for loop (marginally) quicker than .mapply
are_dispatch <- function(
    ..., .fn, .n_args, .all = FALSE, .n = NULL, .finite = NULL) {
  check_dots_unnamed()
  check_all_arg(.all)

  nms <- enexprs(...)
  args <- list2(...)

  if (!is.null(.n)) .n <- prep_are_arg(.n, nms, ...length())
  if (!is.null(.finite)) .finite <- prep_are_arg(.finite, nms, ...length())

  logi <- c()
  fn_call <- switch(.n_args,
    call2(.fn, quote(args[[i]])),
    call2(.fn, quote(args[[i]]), n = quote(.n[[i]])),
    call2(
      .fn,
      quote(args[[i]]),
      n = quote(.n[[i]]),
      finite = quote(.finite[[i]])
    )
  )

  for (i in seq_along(args)) {
    logi[i] <- eval_tidy(fn_call)
  }

  if (isTRUE(.all)) {
    all(logi)
  } else {
    names2(logi) <- nms
    logi
  }
}

prep_are_arg <- function(arg, dot_names, dot_length, darg = caller_arg(arg)) {
  if (!is_vector(arg)) {
    abort(
      "{.var {darg}} must be an atomic vector or a list.",
      call = caller_env(2)
    )
  }

  if (is_named(arg)) {
    dot_names <- vapply(dot_names, as_label, character(1))
    check_arg_names_dot_names(dot_names, names(arg), darg)
    key_val_list(dot_names, arg)
  } else {
    vec_recycle(arg, dot_length, x_arg = darg, call = caller_env(2))
  }
}

key_val_list <- function(keys, vals) {
  vals <- as.list(vals)
  x <- lapply(keys, function(k) vals[[k]])
  names(x) <- keys
  x
}

check_all_arg <- function(.all, n = 2) {
  if (!is_bool(.all)) {
    abort("{.var .all} must be boolean.", call = caller_env(n))
  }
}

check_arg_names_dot_names <- function(dot_names, arg_names, darg) {
  u_length <- function(.x) length(unique(.x))
  le_a <- length(arg_names)
  le_d <- length(dot_names)

  if (le_a > le_d) {
    abort(
      "More named elements present in {.var {darg}} than dot arguments.",
      call = caller_env(3)
    )
  }

  if (le_a != u_length(arg_names)) { # slightly faster than any(duplicated())
    abort(
      "When using a named {.cls vector/list} for {.var {darg}}, the names must be unique.",
      call = caller_env(3)
    )
  }
  if (le_d != u_length(dot_names)) {
    abort(
      "When using a named {.cls vector/list} for {.var {darg}}, the dot arguments must be uniquely named.",
      call = caller_env(3)
    )
  }

  if (any(!arg_names %in% dot_names)) {
    problem_args <- arg_names[!arg_names %in% dot_names]
    abort(
      c(
        "The names of {.var {darg}} must match the dots arguments given.",
        "x" = "{.var {problem_args}} not found in given arguments: {.var {dot_names}}."
      ),
      call = caller_env(3)
    )
  }
}
