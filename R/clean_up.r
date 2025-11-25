# inspiration from tidyselect/R/vars.R

favr_env <- env(old = list(), caller_fn = "")

clean_favr_env_on_exit <- function() {
  favr_env$old <- list()
  favr_env$caller_fn <- ""
}

add_caller_fn <- function(fn) {
  favr_env$caller_fn <- fn
}

collect_old_value <- function(var_name, env) {
  nms <- names(favr_env$old)
  if (is.null(nms)) {
    favr_env$old[[var_name]] <- env[[var_name]]
  } else if (!var_name %in% nms) {
    favr_env$old[[var_name]] <- env[[var_name]]
  }
}

restore_old_values <- function(env) {
  vars <- names(favr_env$old)
  for (n in seq_along(favr_env$old)) {
    assign(
      x = vars[n],
      value = favr_env$old[[n]],
      pos = env
    )
  }

  clean_favr_env_on_exit()
}

#--
