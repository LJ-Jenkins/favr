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
