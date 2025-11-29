#' Is object named?
#'
#' @description
#'
#' Wrappers around [rlang] predicates that allow multiple
#' objects to be passed. The following documentation is adapted
#' from the [rlang] documentation:
#'
#' * `are_named()` is a scalar predicate that checks that objects in `...`
#'    have a `names` attribute and that none of the names are missing or empty
#'   (`NA` or `""`).
#'
#' * `are_named2()` is like `are_named()` but always returns `TRUE` for
#'   empty vectors, even those that don't have a `names` attribute.
#'   In other words, it tests for the property that each element of a
#'   vector is named. `are_named2()` composes well with [names2()]
#'   whereas `are_named()` composes with `names()`.
#'
#' * `have_names()` is a vectorised variant.
#'
#' @inheritParams are_empty
#' @return `are_named()` and `are_named2()` return a named logical,
#' or unnamed boolean if `.all` is `TRUE`. `have_names()` is vectorised
#' and returns a list of logical vectors whhere each is as long as the
#' input object. When `.all` is `TRUE` for `have_names()`, all logical
#' vectors are collapsed and a boolean is returned.
#' @seealso [are-bare-type-predicates] [is_named]
#' @examples
#' # are_named() is a scalar predicate about the whole vector of names:
#' x <- c(a = 1, b = 2)
#' are_named(x, c(a = 1, 2))
#' are_named(x, c(a = 1, 2), .all = TRUE)
#'
#' # Unlike are_named2(), are_named() returns `FALSE` for empty vectors
#' # that don't have a `names` attribute.
#' are_named(list(), vector())
#' are_named2(list(), vector())
#'
#' # have_names() is vectorised
#' y <- c(a = 1, 2)
#' have_names(x, y, c(a = 1, 2, 3))
#' have_names(x, y, c(a = 1, 2, 3), .all = TRUE)
#'
#' # Empty and missing names are treated as invalid:
#' invalid <- set_names(letters[1:5])
#' names(invalid)[1] <- ""
#' names(invalid)[3] <- NA
#'
#' are_named(invalid)
#' have_names(invalid)
#'
#' # A data frame normally has valid, unique names
#' # but a matrix usually doesn't because the names
#' # are stored in a different attribute.
#' mat <- matrix(1:4, 2)
#' colnames(mat) <- c("a", "b")
#' are_named(mtcars, mat)
#' have_names(mtcars, mat)
#' @export
are_named <- function(..., .all = FALSE) {
  are_dispatch(..., .fn = is_named, .n_args = 1, .all = .all)
}

#' @rdname are_named
#' @export
are_named2 <- function(..., .all = FALSE) {
  are_dispatch(..., .fn = is_named2, .n_args = 1, .all = .all)
}

#' @rdname are_named
#' @export
have_names <- function(..., .all = FALSE) {
  check_dots_unnamed()
  check_all_arg(.all, n = 1)

  nms <- enexprs(...)
  args <- list2(...)

  logi <- list()
  for (i in seq_along(args)) {
    logi[[i]] <- have_name(args[[i]])
  }

  if (isTRUE(.all)) {
    all(unlist(logi))
  } else {
    names2(logi) <- nms
    logi
  }
}

#--
