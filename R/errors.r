favr_error_handler <- function(
    cnd, caller_fn, expr, arg = NULL, msg = NULL, call = NULL) {
  expr <- format_inline("In argument: {.var {expr}}.")
  if (cnd_inherits(cnd, "rlang_error") && !cnd_inherits(cnd, "favr_error")) {
    rlang_inherited_body <- strsplit(cnd_message(cnd), "\n")[[1]] %nm% "!"
    abort_favr(
      body = c(i = expr, rlang_inherited_body),
      caller_fn = caller_fn,
      call = call
    )
  } else {
    favr_message_body <- cnd_bullets(
      cnd,
      caller_fn = caller_fn,
      expr = expr,
      arg = arg,
      msg = msg
    )
    abort_favr(
      body = favr_message_body,
      caller_fn = caller_fn,
      call = call
    )
  }
}

cnd_bullets <- function(
    cnd,
    caller_fn,
    expr,
    arg = NULL,
    name = NULL,
    msg = NULL) {
  c(
    "i" = if (!is.null(arg)) format_inline("For named element: {.var {arg}}."),
    "i" = expr,
    "!" = msg %||% if (cnd_inherits(cnd, "favr_error")) {
      unname(cnd_body(cnd))
    } else {
      unname(cnd_header(cnd)) %le0% cnd$message |> as_sentence()
    },
    "x" = if (cnd_inherits(cnd, "favr_error")) {
      cnd_footer(cnd)
    } else {
      unname(cnd_body(cnd)) |> full_stop()
    }
  )
}

#--

abort_favr <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort(
    message,
    class = c(class, "favr_error"),
    ...,
    call = call
  )
}

#' @export
cnd_header.favr_error <- function(cnd, ...) {
  c(NULL = format_inline("{.strong Caused by error in {.fn {cnd$caller_fn}}}."))
}

#--

abort_s3_not_applicable <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort_favr(
    message,
    class = c(class, "favr_error_s3_not_applicable"),
    ...,
    call = call
  )
}

#' @export
cnd_body.favr_error_s3_not_applicable <- function(cnd, ...) {
  format_inline("Data mask {.var {cnd$darg}} must be of class {.cls {cnd$expected_class}}{cnd$schema}.") |> excl()
}

#--

abort_not_class <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort_favr(
    message,
    class = c(class, "favr_error_not_class"),
    ...,
    call = call
  )
}

#' @export
cnd_body.favr_error_not_class <- function(cnd, ...) {
  format_inline("{cnd$arg %||% 'Argument'} must be {.cls {or(cnd$expected)}}, not {.cls {cnd$given}}.") |> excl()
}

#--

abort_logi_returned <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort_favr(
    message,
    class = c(class, "favr_error_logi_returned"),
    ...,
    call = call
  )
}

#' @export
cnd_body.favr_error_logi_returned <- function(cnd, ...) {
  format_inline("Returned {.var {cnd$returned}}.")
}

#' @export
cnd_footer.favr_error_logi_returned <- function(cnd, ...) {
  if (!is.null(cnd$named_ele)) {
    falses <- names(cnd$named_ele[which(cnd$named_ele == FALSE)])
    nas <- names(cnd$named_ele[which(is.na(cnd$named_ele))])
    paste(
      if (length(falses) != 0) {
        format_inline("{.var {falses}} {?is/are} `FALSE`.")
      },
      if (length(nas) != 0) {
        format_inline("{.var {nas}} {?is/are} `NA`.")
      }
    )
  } else {
    NULL
  }
}

#--

abort_empty_vector <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort_favr(
    message,
    class = c(class, "favr_error_empty_vector"),
    ...,
    call = call
  )
}

#' @export
cnd_body.favr_error_empty_vector <- function(cnd, ...) {
  format_inline("Returned an empty vector.")
}

#--

abort_rhs_formula <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort_favr(
    message,
    class = c(class, "favr_error_rhs_formula"),
    ...,
    call = call
  )
}

#' @export
cnd_body.favr_error_rhs_formula <- function(cnd, ...) {
  format_inline("The rhs of a formula must be a {.cls formula}, {.cls function}, or {.cls call} to {.fn recycle}, {.fn cast}, or {.fn coerce}.")
}

#' @export
cnd_footer.favr_error_rhs_formula <- function(cnd, ...) {
  format_inline("Not a {.cls {cnd$given}}.")
}

#--

abort_not_positive_scalar_integerish <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort_favr(
    message,
    class = c(class, "favr_error_not_positive_scalar_integerish"),
    ...,
    call = call
  )
}

#' @export
cnd_body.favr_error_not_positive_scalar_integerish <- function(cnd, ...) {
  format_inline("{cnd$arg %||% 'Size argument'} must be positive scalar integerish, not {length_or_obj(cnd$size)}.") |> excl()
}

#--

abort_coerce_empty <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort_favr(
    message,
    class = c(class, "favr_error_coerce_empty"),
    ...,
    call = call
  )
}

#' @export
cnd_body.favr_error_coerce_empty <- function(cnd, ...) {
  format_inline("{.fn coerce} must specify either a {.var type} and/or a {.var size}.")
}

#--

abort_recycle_not_list <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort_favr(
    message,
    class = c(class, "favr_error_recycle_not_list"),
    ...,
    call = call
  )
}

#' @export
cnd_body.favr_error_recycle_not_list <- function(cnd, ...) {
  format_inline("Recycling is only implemented for {.cls list} objects.")
}

#--

abort_size_arg <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort_favr(
    message,
    class = c(class, "favr_error_size_arg"),
    ...,
    call = call
  )
}

#' @export
cnd_body.favr_error_size_arg <- function(cnd, ...) {
  format_inline("Object {.var {cnd$arg}} is of {.pkg vctrs} size {.var {cnd$actual_size}}, not {.var {cnd$expected_size}}.") |> excl()
}

#--

abort_names_not_present <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort_favr(
    message,
    class = c(class, "favr_error_names_not_present"),
    ...,
    call = call
  )
}

#' @export
cnd_body.favr_error_names_not_present <- function(cnd, ...) {
  format_inline("Named {qty(cnd$not_found)}element{?s} {.var {cnd$not_found}} not found in data mask {.var {cnd$mask}}.") |> excl()
}

#--

abort_env <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort_favr(
    message,
    class = c(class, "favr_error_env"),
    ...,
    call = call
  )
}

#' @export
cnd_body.favr_error_env <- function(cnd, ...) {
  format_inline("{.var {cnd$env_name}} must be a {.cls environment}.") |> excl()
}

#--

abort_args_unnamed <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort_favr(
    message,
    class = c(class, "favr_error_args_unnamed"),
    ...,
    call = call
  )
}

#' @export
cnd_body.favr_error_args_unnamed <- function(cnd, ...) {
  i <- length(cnd$i)
  format_inline("{qty(i)}Argument{?s} {?is/are} not named with object{?s} to {cnd$action} in {qty(i)}position{?s} {.var {cnd$i}}.") |> excl()
}

#--

abort_args_env <- function(
    message = NULL,
    class = NULL,
    ...,
    call = caller_env()) {
  abort_favr(
    message,
    class = c(class, "favr_error_args_env"),
    ...,
    call = call
  )
}

#' @export
cnd_body.favr_error_args_env <- function(cnd, ...) {
  format_inline("{qty(cnd$not_found)}Object{?s} {.var {cnd$not_found}} {?is/are} not found in the {.var .env} environment specified.") |> excl()
}

#--

#' @export
cnd_body.favr_error_fn_called_not_in_cast_if_not <- function(cnd, ...) {
  format_inline("{.fn lossy} must be used within {.fn cast_if_not} calls.")
}

#' @export
cnd_body.favr_error_fn_called_not_in_enforce_schema <- function(cnd, ...) {
  format_inline("{.fn {cnd$fn}} must be used within {.fn enforce} or {.fn schema} calls.")
}

#--

#' @export
cnd_body.favr_error_not_c <- function(cnd, ...) {
  format_inline("The lhs of formulas within {.fn enforce} must be bare variable names or calls to {.fn c}.")
}
