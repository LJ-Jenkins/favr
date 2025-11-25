#' favr casting and recycling helpers
#'
#' @description
#' These function signal to favr functions to undergo casting,
#' lossy casting, and/or recycling. Each can only be used wihtin calls
#' to specific favr functions and will error if used outside them.
#' Specifically:
#' *   `lossy()`: used within [favr::cast_if_not()] for lossy casting.
#' *   `cast()`: used within [favr::enforce()] and [favr::schema()] for casting.
#' *   `recycle()`: used within [favr::enforce()] and [favr::schema()] for
#'                  recycling.
#' *   `coerce()`: used within [favr::enforce()] and [favr::schema()] for
#'                 casting and recycling.
#'
#' @keywords internal
#' @name favr_casting_recycling_helpers
#' @examples
#' x <- 1.5
#' cast_if_not(x = lossy(integer()))
#' class(x) # integer
#'
#' enforce(x ~ list(cast(double()), recycle(5)))
#' class(x) # numeric
#' length(x) # 5
#'
#' x <- 1.5
#' enforce(x ~ coerce(type = integer(), size = 5, lossy = TRUE))
#' class(x) # integer
#' length(x) # 5

#' @rdname favr_casting_recycling_helpers
#' @export
lossy <- function(x) {
  check_call_is("cast_if_not")

  attr(x, "favr:::lossy") <- TRUE
  x
}

#' @rdname favr_casting_recycling_helpers
#' @export
cast <- function(x, lossy = FALSE) {
  check_call_is(c("enforce", "schema"), "cast")

  x <- list(type = x, lossy = lossy)
  class(x) <- "favr:::arg"
  attr(x, "arg_cast") <- TRUE
  x
}

#' @rdname favr_casting_recycling_helpers
#' @export
recycle <- function(x) {
  check_call_is(c("enforce", "schema"), "recycle")

  check_positive_scalar_integerish(x, arg = "`recycle()` argument")
  x <- list(size = x)
  class(x) <- "favr:::arg"
  attr(x, "arg_recycle") <- TRUE
  x
}

#' @rdname favr_casting_recycling_helpers
#' @export
coerce <- function(type = NULL, size = NULL, lossy = FALSE) {
  check_call_is(c("enforce", "schema"), "coerce")

  if (is.null(type) && is.null(size)) {
    abort_coerce_empty()
  }

  x <- list(lossy = lossy)
  if (!is.null(type)) {
    x$type <- type
    attr(x, "arg_cast") <- TRUE
  }

  if (!is.null(size)) {
    check_positive_scalar_integerish(size)
    x$size <- size
    attr(x, "arg_recycle") <- TRUE
  }

  class(x) <- "favr:::arg"
  x
}

vec_cast_lossy <- function(x, to, x_arg, allow_lossy = FALSE) {
  if (isTRUE(allow_lossy)) {
    # Use allow_lossy_cast() to handle potential lossy conversions
    allow_lossy_cast(vec_cast(x, to, x_arg = x_arg))
  } else {
    vec_cast(x, to, x_arg = x_arg)
  }
}
