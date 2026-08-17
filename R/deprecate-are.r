#' Type predicates
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions were deprecated as they offer little benefit over `lapply()`.
#'
#' Wrappers around [rlang][rlang::rlang-package] type predicates that
#' allow multiple objects to be passed. The following documentation is adapted
#' from the rlang [documentation][rlang::type-predicates]:
#'
#' These type predicates aim to make type testing in R more
#' consistent. They are wrappers around [base::typeof()], so operate
#' at a level beneath S3/S4 etc.
#'
#' Compared to base R functions:
#'
#' * The predicates for vectors include the `.n` argument for
#'   pattern-matching on the vector length.
#'
#' * Unlike `is.atomic()` in R < 4.4.0, `are_atomic()` does not return `TRUE`
#'   for `NULL`. Starting in R 4.4.0 `is.atomic(NULL)` returns FALSE.
#'
#' * Unlike `is.vector()`, `are_vector()` tests if an object is an
#'   atomic vector or a list. `is.vector` checks for the presence of
#'   attributes (other than name).
#'
#' @param ... Objects to be tested.
#' @param .n Expected lengths of the vectors.
#' @param .finite Whether all values of the vectors are finite. The
#' non-finite values are `NA`, `Inf`, `-Inf` and `NaN`. Setting this
#' to something other than `NULL` can be expensive because the whole
#' vector needs to be traversed and checked.
#' @param .all If `TRUE`, return boolean of whether all arguments returned
#' `TRUE`.
#' @return Named logical, or unnamed boolean if `.all` is `TRUE`.
#' @details The optional inputs of `.n` and `.finite` can be given inputs
#' that map to the arguments in `...`. If a unnamed vector/list, the input must
#' either be the same length as the number of arguments given to `...`, or
#' length 1: which is then recycled to the number number of arguments
#' given to `...`. Alternatively, a named vector/list can be given, where
#' the values for matching named elements are passed to the type predicate,
#' but unmatched names are passed NULL.
#' @seealso [are-bare-type-predicates] [are-scalar-type-predicates]
#' @name are-type-predicates
#' @keywords internal
#' @examples
#' x <- 1
#' y <- list()
#' z <- mean
#'
#' are_list(x, y, z, list(1))
#'
#' # `.all` can be given to test if all inputs
#' # evaluate to TRUE
#' are_list(x, y, z, list(1), .all = TRUE)
#'
#' # scalar inputs to `.n` and `.finite` are
#' # recycled to number of inputs
#' are_list(x, y, z, list(1), .n = 1)
#'
#' # inputs to `.n` and `.finite` matching the
#' # number of inputs are applied sequentially
#' are_list(x, y, z, list(1), .n = c(1, 0, 1, 2))
#'
#' # named inputs to `.n` and `.finite` are applied
#' # to the matching input names, with the other inputs
#' # being given NULL
#' are_list(x, y, z, list(1), .n = c(y = 1, "list(1)" = 2))
NULL

#' @export
#' @rdname are-type-predicates
are_list <- function(..., .n = NULL, .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_list()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_list, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-type-predicates
are_atomic <- function(..., .n = NULL, .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_atomic()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_atomic, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-type-predicates
are_vector <- function(..., .n = NULL, .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_vector()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_vector, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-type-predicates
are_integer <- function(..., .n = NULL, .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_integer()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_integer, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-type-predicates
are_double <- function(..., .n = NULL, .finite = NULL, .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_double()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(
    ...,
    .fn = is_double,
    .n_args = 3,
    .all = .all,
    .n = .n,
    .finite = .finite
  )
}

#' @export
#' @rdname are-type-predicates
are_complex <- function(..., .n = NULL, .finite = NULL, .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_complex()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(
    ...,
    .fn = is_complex,
    .n_args = 3,
    .all = .all,
    .n = .n,
    .finite = .finite
  )
}

#' @export
#' @rdname are-type-predicates
are_character <- function(..., .n = NULL, .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_character()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_character, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-type-predicates
are_logical <- function(..., .n = NULL, .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_logical()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_logical, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-type-predicates
are_raw <- function(..., .n = NULL, .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_raw()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_raw, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-type-predicates
are_bytes <- function(..., .n = NULL, .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_bytes()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_bytes, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-type-predicates
are_null <- function(..., .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_null()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_null, .n_args = 1, .all = .all)
}

#--

#' Scalar type predicates
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions were deprecated as they offer little benefit over `lapply()`.
#'
#' Wrappers around [rlang][rlang::rlang-package] scalar type predicates that
#' allow multiple objects to be passed. The following documentation is adapted
#' from the rlang [documentation][rlang::scalar-type-predicates]:
#'
#' These predicates check for a given type and whether the vector is
#' "scalar", that is, of length 1.
#'
#' In addition to the length check, `are_string()` and `are_bool()`
#' return `FALSE` if their input is missing. This is useful for
#' type-checking arguments, when your function expects a single string
#' or a single `TRUE` or `FALSE`.
#'
#' @param ... Objects to be tested.
#' @param .string A string/character vector to compare to the inputs.
#' @param .all Whether to return if all arguments are TRUE.
#' @inherit are-type-predicates return
#' @details The optional input of `.string` can be given character vectors
#' that map to the arguments in `...`. If unnamed vector/list, the input must
#' either be the same length as the number of arguments given to `...`, or
#' length 1: which is then recycled to the number number of arguments
#' given to `...`. Alternatively, a named vector/list can be given, where
#' the values for matching named elements are passed to the type predicate,
#' but unmatched names are passed NULL. List inputs can pass different
#' character vectors for each dot argument. When a character vector is given for
#' a single argument, `TRUE` is returned if at least one element is equal.
#' @seealso [are-type-predicates], [are-bare-type-predicates]
#' @name are-scalar-type-predicates
#' @keywords internal
#' @examples
#' x <- 1
#' y <- list()
#' z <- mean
#'
#' are_scalar_list(x, y, z, list(1))
#'
#' # `.all` can be given to test if all inputs
#' # evaluate to TRUE
#' are_list(x, y, z, list(1), .all = TRUE)
NULL

#' @export
#' @rdname are-scalar-type-predicates
are_scalar_list <- function(..., .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_scalar_list()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_scalar_list, .n_args = 1, .all = .all)
}

#' @export
#' @rdname are-scalar-type-predicates
are_scalar_atomic <- function(..., .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_scalar_atomic()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_scalar_atomic, .n_args = 1, .all = .all)
}

#' @export
#' @rdname are-scalar-type-predicates
are_scalar_vector <- function(..., .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_scalar_vector()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_scalar_vector, .n_args = 1, .all = .all)
}

#' @export
#' @rdname are-scalar-type-predicates
are_scalar_integer <- function(..., .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_scalar_integer()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_scalar_integer, .n_args = 1, .all = .all)
}

#' @export
#' @rdname are-scalar-type-predicates
are_scalar_double <- function(..., .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_scalar_double()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_scalar_double, .n_args = 1, .all = .all)
}

#' @export
#' @rdname are-scalar-type-predicates
are_scalar_complex <- function(..., .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_scalar_complex()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_scalar_complex, .n_args = 1, .all = .all)
}

#' @export
#' @rdname are-scalar-type-predicates
are_scalar_character <- function(..., .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_scalar_character()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_scalar_character, .n_args = 1, .all = .all)
}

#' @export
#' @rdname are-scalar-type-predicates
are_string <- function(..., .string = NULL, .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_string()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  check_dots_unnamed()
  check_all_arg(.all, n = 1)

  nms <- enexprs(...)
  args <- list2(...)

  if (!is.null(.string)) .string <- prep_are_arg(.string, nms, ...length())

  logi <- c()
  for (i in seq_along(args)) {
    logi[i] <- is_string(args[[i]], string = .string[[i]])
  }

  if (isTRUE(.all)) {
    all(logi)
  } else {
    names2(logi) <- nms
    logi
  }
}

#' @export
#' @rdname are-scalar-type-predicates
are_scalar_logical <- function(..., .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_scalar_logical()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_scalar_logical, .n_args = 1, .all = .all)
}

#' @export
#' @rdname are-scalar-type-predicates
are_bool <- function(..., .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_bool()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_bool, .n_args = 1, .all = .all)
}

#' @export
#' @rdname are-scalar-type-predicates
are_scalar_raw <- function(..., .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_scalar_raw()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_scalar_raw, .n_args = 1, .all = .all)
}

#' @export
#' @rdname are-scalar-type-predicates
are_scalar_bytes <- function(..., .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_scalar_bytes()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_scalar_bytes, .n_args = 1, .all = .all)
}

#--

#' Bare type predicates
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions were deprecated as they offer little benefit over `lapply()`.
#'
#' Wrappers around [rlang][rlang::rlang-package] type predicates that
#' allow multiple objects to be passed. The following documentation is adapted
#' from the rlang [documentation][rlang::bare-type-predicates]:
#'
#' These predicates check for a given type but only return `TRUE` for
#' bare R objects. Bare objects have no class attributes. For example,
#' a data frame is a list, but not a bare list.
#'
#' * The predicates for vectors include the `.n` argument for
#'   pattern-matching on the vector length.
#'
#' * Like `are_atomic()` and unlike base R `is.atomic()` for R < 4.4.0,
#'   `are_bare_atomic()` does not return `TRUE` for `NULL`. Starting in
#'   R 4.4.0, `is.atomic(NULL)` returns FALSE.
#'
#' * Unlike base R `is.numeric()`, `are_bare_double()` only returns
#'   `TRUE` for floating point numbers.
#'
#' @param ... Objects to be tested.
#' @param .n Expected lengths of the vectors.
#' @param .all Whether to return if all arguments are TRUE.
#' @inherit are-type-predicates return
#' @details The optional input of `.n` can be given values
#' that map to the arguments in `...`. If a unnamed vector/list, the input must
#' either be the same length as the number of arguments given to `...`, or
#' length 1: which is then recycled to the number number of arguments
#' given to `...`. Alternatively, a named vector/list can be given, where
#' the values for matching named elements are passed to the type predicate,
#' but unmatched names are passed NULL.
#' @seealso [are-type-predicates], [are-scalar-type-predicates]
#' @name are-bare-type-predicates
#' @keywords internal
#' @examples
#' x <- 1
#' y <- list()
#' class(y) <- c("my_class", class(y))
#' z <- mean
#'
#' are_bare_list(x, y, z, list(1))
#'
#' # `.all` can be given to test if all inputs
#' # evaluate to TRUE
#' are_bare_list(x, y, z, list(1), .all = TRUE)
#'
#' # scalar inputs to `.n` are recycled to number of inputs
#' are_bare_list(x, y, z, list(1), .n = 2)
#'
#' # inputs to `.n` matching the number of inputs
#' # are applied sequentially
#' are_bare_list(list(), y, list(1, 2, 3), list(1), .n = c(0, 0, 3, 1))
#'
#' # named inputs to `.n` are applied to the matching input
#' # names, with the other inputs being given NULL
#' x <- list()
#' are_bare_list(x, y, list(1, 2, 3), list(1), .n = c(x = 5, "list(1)" = 2))
NULL

#' @export
#' @rdname are-bare-type-predicates
are_bare_list <- function(..., .n = NULL, .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_bare_list()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_bare_list, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-bare-type-predicates
are_bare_atomic <- function(..., .n = NULL, .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_bare_atomic()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_bare_atomic, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-bare-type-predicates
are_bare_vector <- function(..., .n = NULL, .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_bare_vector()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_bare_vector, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-bare-type-predicates
are_bare_integer <- function(..., .n = NULL, .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_bare_integer()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_bare_integer, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-bare-type-predicates
are_bare_double <- function(..., .n = NULL, .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_bare_double()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_bare_double, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-bare-type-predicates
are_bare_complex <- function(..., .n = NULL, .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_bare_complex()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_bare_complex, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-bare-type-predicates
are_bare_character <- function(..., .n = NULL, .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_bare_character()",
    details = "Please use `lapply()` with an anonmous function instead."
  )
  are_dispatch(..., .fn = is_bare_character, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-bare-type-predicates
are_bare_string <- function(..., .n = NULL, .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_bare_string()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_bare_string, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-bare-type-predicates
are_bare_logical <- function(..., .n = NULL, .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_bare_logical()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_bare_logical, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-bare-type-predicates
are_bare_raw <- function(..., .n = NULL, .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_bare_raw()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_bare_raw, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-bare-type-predicates
are_bare_bytes <- function(..., .n = NULL, .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_bare_bytes()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_bare_bytes, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-bare-type-predicates
are_bare_numeric <- function(..., .n = NULL, .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_bare_numeric()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_bare_numeric, .n_args = 2, .all = .all, .n = .n)
}

#--

#' Are objects empty vectors or NULL?
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions were deprecated as they offer little benefit over `lapply()`.
#' @param ... Objects to be tested.
#' @param .all Whether to return if all arguments are TRUE.
#' @inherit are-type-predicates return
#' @seealso [is_empty][rlang::is_empty]
#' @examples
#' x <- 1
#' y <- NULL
#' z <- list()
#'
#' are_empty(x, y, z, NULL)
#'
#' are_empty(x, y, z, NULL, .all = TRUE)
#'
#' are_empty(list(NULL))
#' @keywords internal
#' @export
are_empty <- function(..., .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_empty()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_empty, .n_args = 1, .all = .all)
}

#--

#' Are objects TRUE or FALSE?
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions were deprecated as they offer little benefit over `lapply()`.
#'
#' Test if any number of inputs are TRUE or FALSE.
#' Inputs are passed to [isTRUE] or [isFALSE].
#' @inheritParams are_empty
#' @inherit are-type-predicates return
#' @seealso [isTRUE] [isFALSE]
#' @examples
#' x <- TRUE
#' y <- 1
#' z <- mean
#'
#' are_true(x, y, z, TRUE, 0)
#'
#' are_true(x, y, z, TRUE, 0, .all = TRUE)
#'
#' are_false(x, y, z, TRUE, 0)
#'
#' are_false(x, y, z, TRUE, 0, .all = TRUE)
#' @keywords internal
#' @export
are_true <- function(..., .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_true()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = isTRUE, .n_args = 1, .all = .all)
}

#' @export
#' @rdname are_true
are_false <- function(..., .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_false()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = isFALSE, .n_args = 1, .all = .all)
}

#--

#' Are vectors integer-like?
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions were deprecated as they offer little benefit over `lapply()`.
#'
#' Wrappers around [rlang][rlang::rlang-package] type predicates that allow
#' multiple objects to be passed. The following documentation is adapted
#' from the rlang [documentation][rlang::is_integerish]:
#'
#' These predicates check whether R considers a number vector to be
#' integer-like, according to its own tolerance check (which is in
#' fact delegated to the C library). This function is not adapted to
#' data analysis, see the help for [base::is.integer()] for examples
#' of how to check for whole numbers.
#'
#' Things to consider when checking for integer-like doubles:
#'
#' * This check can be expensive because the whole double vector has
#'   to be traversed and checked.
#'
#' * Large double values may be integerish but may still not be
#'   coercible to integer. This is because integers in R only support
#'   values up to `2^31 - 1` while numbers stored as double can be
#'   much larger.
#'
#' @inheritParams are-type-predicates
#' @inherit are-type-predicates return details
#' @seealso [are_bare_numeric] for testing whether an object is a
#' base numeric type (a bare double or integer vector).
#' @export
#' @examples
#' x <- 10L
#' y <- 10.0
#' z <- 10.000001
#'
#' are_integerish(x, y, z, TRUE)
#'
#' #' # `.all` can be given to test if all inputs
#' # evaluate to TRUE
#' are_integerish(x, y, z, TRUE, .all = TRUE)
#'
#' # scalar inputs to `.n` and `.finite` are
#' # recycled to number of inputs
#' are_integerish(x, y, z, TRUE, .n = 2)
#'
#' # inputs to `.n` and `.finite` matching the
#' # number of inputs are applied sequentially
#' are_integerish(x, y, z, TRUE, .n = c(1, 2, 1, 1))
#'
#' # named inputs to `.n` and `.finite` are applied
#' # to the matching input names, with the other inputs
#' # being given NULL
#' are_integerish(x, y, z, TRUE, .n = c(y = 2, "TRUE" = 1))
#' @keywords internal
#' @export
are_integerish <- function(..., .n = NULL, .finite = NULL, .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_integerish()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(
    ...,
    .fn = is_integerish,
    .n_args = 3,
    .all = .all,
    .n = .n,
    .finite = .finite
  )
}

#' @export
#' @rdname are_integerish
are_scalar_integerish <- function(
  ..., .finite = NULL, .all = FALSE
) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_scalar_integerish()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  check_dots_unnamed()
  check_all_arg(.all)

  nms <- enexprs(...)
  args <- list2(...)

  if (!is.null(.finite)) .finite <- prep_are_arg(.finite, nms, ...length())

  logi <- c()
  fn_call <- call2(is_scalar_integerish, quote(args[[i]]), finite = quote(.finite[[i]]))

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

#' @export
#' @rdname are_integerish
are_bare_integerish <- function(..., .n = NULL, .finite = NULL, .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_bare_integerish()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(
    ...,
    .fn = is_bare_integerish,
    .n_args = 3,
    .all = .all,
    .n = .n,
    .finite = .finite
  )
}

#' Are objects named?
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions were deprecated as they offer little benefit over `lapply()`.
#'
#' Wrappers around [rlang][rlang::rlang-package] predicates that allow multiple
#' objects to be passed. The following documentation is adapted
#' from the rlang [documentation][rlang::is_named]:
#'
#' * `are_named()` is a scalar predicate that checks that objects in `...`
#'    have a `names` attribute and that none of the names are missing or empty
#'   (`NA` or `""`).
#'
#' * `are_named2()` is like `are_named()` but always returns `TRUE` for
#'   empty vectors, even those that don't have a `names` attribute.
#'   In other words, it tests for the property that each element of a
#'   vector is named. `are_named2()` composes well with `names2()`
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
#' @seealso [are-bare-type-predicates] [rlang::is_named]
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
#' invalid <- setNames(letters[1:5], letters[1:5])
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
#' @keywords internal
#' @export
are_named <- function(..., .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_named()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_named, .n_args = 1, .all = .all)
}

#' @rdname are_named
#' @export
are_named2 <- function(..., .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "are_named2()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

  are_dispatch(..., .fn = is_named2, .n_args = 1, .all = .all)
}

#' @rdname are_named
#' @export
have_names <- function(..., .all = FALSE) {
  lifecycle::deprecate_soft(
    "1.1.0",
    "have_names()",
    details = "Please use `lapply()` with an anonmous function instead."
  )

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

# are_utils.r

# for loop (marginally) quicker than .mapply
are_dispatch <- function(
  ..., .fn, .n_args, .all = FALSE, .n = NULL, .finite = NULL
) {
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
