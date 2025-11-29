#' Type predicates
#'
#' @description
#'
#' Wrappers around [rlang] type predicates that allow multiple
#' objects to be passed. The following documentation is adapted
#' from the [rlang] documentation:
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
#' @seealso [are-bare-type-predicates] [are-scalar-type-predicates] [type-predicates]
#' @name are-type-predicates
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
  are_dispatch(..., .fn = is_list, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-type-predicates
are_atomic <- function(..., .n = NULL, .all = FALSE) {
  are_dispatch(..., .fn = is_atomic, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-type-predicates
are_vector <- function(..., .n = NULL, .all = FALSE) {
  are_dispatch(..., .fn = is_vector, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-type-predicates
are_integer <- function(..., .n = NULL, .all = FALSE) {
  are_dispatch(..., .fn = is_integer, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-type-predicates
are_double <- function(..., .n = NULL, .finite = NULL, .all = FALSE) {
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
  are_dispatch(..., .fn = is_character, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-type-predicates
are_logical <- function(..., .n = NULL, .all = FALSE) {
  are_dispatch(..., .fn = is_logical, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-type-predicates
are_raw <- function(..., .n = NULL, .all = FALSE) {
  are_dispatch(..., .fn = is_raw, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-type-predicates
are_bytes <- function(..., .n = NULL, .all = FALSE) {
  are_dispatch(..., .fn = is_bytes, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-type-predicates
are_null <- function(..., .all = FALSE) {
  are_dispatch(..., .fn = is_null, .n_args = 1, .all = .all)
}

#--

#' Scalar type predicates
#'
#' @description
#'
#' Wrappers around [rlang] scalar type predicates that allow multiple
#' objects to be passed. The following documentation is adapted
#' from the [rlang] documentation:
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
#' @seealso [are-type-predicates], [are-bare-type-predicates] [scalar-type-predicates]
#' @name are-scalar-type-predicates
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
  are_dispatch(..., .fn = is_scalar_list, .n_args = 1, .all = .all)
}

#' @export
#' @rdname are-scalar-type-predicates
are_scalar_atomic <- function(..., .all = FALSE) {
  are_dispatch(..., .fn = is_scalar_atomic, .n_args = 1, .all = .all)
}

#' @export
#' @rdname are-scalar-type-predicates
are_scalar_vector <- function(..., .all = FALSE) {
  are_dispatch(..., .fn = is_scalar_vector, .n_args = 1, .all = .all)
}

#' @export
#' @rdname are-scalar-type-predicates
are_scalar_integer <- function(..., .all = FALSE) {
  are_dispatch(..., .fn = is_scalar_integer, .n_args = 1, .all = .all)
}

#' @export
#' @rdname are-scalar-type-predicates
are_scalar_double <- function(..., .all = FALSE) {
  are_dispatch(..., .fn = is_scalar_double, .n_args = 1, .all = .all)
}

#' @export
#' @rdname are-scalar-type-predicates
are_scalar_complex <- function(..., .all = FALSE) {
  are_dispatch(..., .fn = is_scalar_complex, .n_args = 1, .all = .all)
}

#' @export
#' @rdname are-scalar-type-predicates
are_scalar_character <- function(..., .all = FALSE) {
  are_dispatch(..., .fn = is_scalar_character, .n_args = 1, .all = .all)
}

#' @export
#' @rdname are-scalar-type-predicates
are_string <- function(..., .string = NULL, .all = FALSE) {
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
  are_dispatch(..., .fn = is_scalar_logical, .n_args = 1, .all = .all)
}

#' @export
#' @rdname are-scalar-type-predicates
are_bool <- function(..., .all = FALSE) {
  are_dispatch(..., .fn = is_bool, .n_args = 1, .all = .all)
}

#' @export
#' @rdname are-scalar-type-predicates
are_scalar_raw <- function(..., .all = FALSE) {
  are_dispatch(..., .fn = is_scalar_raw, .n_args = 1, .all = .all)
}

#' @export
#' @rdname are-scalar-type-predicates
are_scalar_bytes <- function(..., .all = FALSE) {
  are_dispatch(..., .fn = is_scalar_bytes, .n_args = 1, .all = .all)
}

#--

#' Bare type predicates
#'
#' @description
#'
#' Wrappers around [rlang] type predicates that allow multiple
#' objects to be passed. The following documentation is adapted
#' from the [rlang] documentation:
#'
#' These predicats check for a given type but only return `TRUE` for
#' bare R objects. Bare objects have no class attributes. For example,
#' a data frame is a list, but not a bare list.
#'
#' * The predicates for vectors include the `.n` argument for
#'   pattern-matching on the vector length.
#'
#' * Like [are_atomic()] and unlike base R `is.atomic()` for R < 4.4.0,
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
#' @seealso [are-type-predicates], [are-scalar-type-predicates] [bare-type-predicates]
#' @name are-bare-type-predicates
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
  are_dispatch(..., .fn = is_bare_list, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-bare-type-predicates
are_bare_atomic <- function(..., .n = NULL, .all = FALSE) {
  are_dispatch(..., .fn = is_bare_atomic, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-bare-type-predicates
are_bare_vector <- function(..., .n = NULL, .all = FALSE) {
  are_dispatch(..., .fn = is_bare_vector, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-bare-type-predicates
are_bare_integer <- function(..., .n = NULL, .all = FALSE) {
  are_dispatch(..., .fn = is_bare_integer, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-bare-type-predicates
are_bare_double <- function(..., .n = NULL, .all = FALSE) {
  are_dispatch(..., .fn = is_bare_double, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-bare-type-predicates
are_bare_complex <- function(..., .n = NULL, .all = FALSE) {
  are_dispatch(..., .fn = is_bare_complex, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-bare-type-predicates
are_bare_character <- function(..., .n = NULL, .all = FALSE) {
  are_dispatch(..., .fn = is_bare_character, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-bare-type-predicates
are_bare_string <- function(..., .n = NULL, .all = FALSE) {
  are_dispatch(..., .fn = is_bare_string, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-bare-type-predicates
are_bare_logical <- function(..., .n = NULL, .all = FALSE) {
  are_dispatch(..., .fn = is_bare_logical, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-bare-type-predicates
are_bare_raw <- function(..., .n = NULL, .all = FALSE) {
  are_dispatch(..., .fn = is_bare_raw, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-bare-type-predicates
are_bare_bytes <- function(..., .n = NULL, .all = FALSE) {
  are_dispatch(..., .fn = is_bare_bytes, .n_args = 2, .all = .all, .n = .n)
}

#' @export
#' @rdname are-bare-type-predicates
are_bare_numeric <- function(..., .n = NULL, .all = FALSE) {
  are_dispatch(..., .fn = is_bare_numeric, .n_args = 2, .all = .all, .n = .n)
}

#--

#' Are objects empty vectors or NULL?
#'
#' @param ... Objects to be tested.
#' @param .all Whether to return if all arguments are TRUE.
#' @inherit are-type-predicates return
#' @seealso [is_empty]
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
#' @export
are_empty <- function(..., .all = FALSE) {
  are_dispatch(..., .fn = is_empty, .n_args = 1, .all = .all)
}

#--

#' Are objects TRUE or FALSE?
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
#' @export
are_true <- function(..., .all = FALSE) {
  are_dispatch(..., .fn = isTRUE, .n_args = 1, .all = .all)
}

#' @export
#' @rdname are_true
are_false <- function(..., .all = FALSE) {
  are_dispatch(..., .fn = isFALSE, .n_args = 1, .all = .all)
}

#--

#' Are vectors integer-like?
#'
#' @description
#'
#' Wrappers around [rlang] type predicates that allow multiple
#' objects to be passed. The following documentation is adapted
#' from the [rlang] documentation:
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
#' @seealso [is_bare_numeric] and [are_bare_numeric] for testing
#' whether an object is a base numeric type (a bare double or integer vector).
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
#' @export
are_integerish <- function(..., .n = NULL, .finite = NULL, .all = FALSE) {
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
    ..., .n = NULL, .finite = NULL, .all = FALSE) {
  are_dispatch(
    ...,
    .fn = is_scalar_integerish,
    .n_args = 3,
    .all = .all,
    .n = .n,
    .finite = .finite
  )
}

#' @export
#' @rdname are_integerish
are_bare_integerish <- function(..., .n = NULL, .finite = NULL, .all = FALSE) {
  are_dispatch(
    ...,
    .fn = is_bare_integerish,
    .n_args = 3,
    .all = .all,
    .n = .n,
    .finite = .finite
  )
}

#--
