# Taken from rlang, modifications for cli formatting.
# All credit to original authors.
type_friendly <- function(x, value = TRUE, length = FALSE) {
  # no missing within package (at present) so skip

  if (is.object(x)) {
    if (inherits(x, "quosure")) {
      type <- "{.cls quosure}"
    } else {
      type <- class(x)[[1L]]
    }
    return(sprintf("a {.cls %s} object", type))
  }

  if (!is_vector(x)) {
    return(as_friendly_type(typeof(x)))
  }

  n_dim <- length(dim(x))

  if (!n_dim) {
    if (!is_list(x) && length(x) == 1) {
      if (is_na(x)) {
        return(switch(typeof(x),
          logical = "{.val {NA}}",
          integer = "an {.cls integer} {.val {NA}}",
          double = if (is.nan(x)) {
            "{.val {NaN}}"
          } else {
            "a {.cls numeric} {.val {NA}}"
          },
          complex = "a {.cls complex} {.val {NA}}",
          character = "a {.cls character} {.val {NA}}",
          unrecognised_type(x)
        ))
      }

      if (value) {
        if (is.numeric(x) && is.infinite(x)) {
          return(show_infinites(x))
        }

        if (is.numeric(x) || is.complex(x)) {
          number <- as.character(round(x, 2))
          what <- if (is.complex(x)) {
            "the {.cls complex} number"
          } else {
            "the number"
          }
          return(paste0(what, " {.val {", number, "}}"))
        }

        return(switch(typeof(x),
          logical = if (x) "{.val {TRUE}}" else "{.val {FALSE}}",
          character = {
            what <- if (nzchar(x)) "the string" else "the empty string"
            paste0(what, " {.val {", str_encode(x, quote = "\""), "}}")
          },
          raw = paste0("the {.cls raw} value {.val {", as.character(x), "}}"),
          unrecognised_type(x)
        ))
      }

      return(switch(typeof(x),
        logical = "a {.cls logical} value",
        integer = "an {.cls integer}",
        double = if (is.infinite(x)) show_infinites(x) else "a {.cls numeric}",
        complex = "a {.cls complex} number",
        character = if (nzchar(x)) "a {.cls character} string" else "\"\"",
        raw = "a {.cls raw} value",
        unrecognised_type(x)
      ))
    }

    if (length(x) == 0) {
      return(switch(typeof(x),
        logical = "an empty {.cls logical} vector",
        integer = "an empty {.cls integer} vector",
        double = "an empty {.cls numeric} vector",
        complex = "an empty {.cls complex} vector",
        character = "an empty {.cls character} vector",
        raw = "an empty {.cls raw} vector",
        list = "an empty {.cls list}",
        unrecognised_type(x)
      ))
    }
  }

  vec_friendly(x, length = length)
}

vec_friendly <- function(x, length = FALSE) {
  if (!is_vector(x)) {
    cli_abort("{.arg x} must be a {.cls vector}.")
  }
  type <- typeof(x)
  n_dim <- length(dim(x))

  add_length <- function(type) {
    if (length && !n_dim) {
      paste0(type, sprintf(" of length {.val {%s}}", length(x)))
    } else {
      type
    }
  }

  if (type == "list") {
    if (n_dim == 0) {
      return(add_length("a {.cls list}"))
    } else if (n_dim == 2) {
      if (is.data.frame(x)) {
        return("a {.cls data.frame}")
      } else {
        return("a {.cls {c('list', 'matrix')}}")
      }
    } else {
      return(sprintf("a {.cls list} %sD {.cls array}", n_dim))
    }
  }

  type <- switch(type,
    logical = "a {.cls logical%s",
    integer = "an {.cls integer%s",
    double = "a {.cls double%s",
    complex = "a {.cls complex%s",
    character = "a {.cls character%s",
    raw = "a {.cls raw%s",
    type = paste0("a {.cls ", type, "%s")
  )

  if (n_dim == 0) {
    kind <- "} vector"
  } else if (n_dim == 2) {
    kind <- "/matrix}"
  } else {
    kind <- sprintf("} %sD {.cls array}", n_dim)
  }
  out <- sprintf(type, kind)

  if (n_dim >= 2) {
    out
  } else {
    add_length(out)
  }
}

as_friendly_type <- function(type) {
  switch(type,
    list = "a {.cls list}",
    NULL = "{.cls NULL}",
    environment = "an {.cls environment}",
    externalptr = "a {.cls pointer}",
    weakref = "a {.cls weak reference}",
    S4 = "an {.cls S4} object",
    name = "a {.cls name}",
    symbol = "a {.cls symbol}",
    language = "a {.cls call}",
    pairlist = "a {.cls pairlist} node",
    expression = "an {.cls expression} vector",
    char = "an internal string ({.cls char})",
    promise = "an internal {.cls promise}",
    ... = "an internal {.cls dots} object",
    any = "an internal {.cls any} object",
    bytecode = "an internal {.cls bytecode} object",
    primitive = "a {.cls primitive} function",
    builtin = "a {.cls builtin} function",
    special = "a {.cls special} function",
    closure = "a {.cls function}",
    type
  )
}

unrecognised_type <- function(x) {
  format_inline("Unrecognised, unsupported type: {.cls {typeof(x)}}")
}

show_infinites <- function(x) {
  if (x > 0) {
    "{.val {Inf}}"
  } else {
    "{.val {-Inf}}"
  }
}

str_encode <- function(x, width = 30, ...) {
  if (nchar(x) > width) {
    x <- substr(x, 1, width - 3)
    x <- paste0(x, "...")
  }
  encodeString(x, ...)
}
