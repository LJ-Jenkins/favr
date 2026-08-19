#' S3 type checks
#'
#' @description
#' Check if inputs are expected `S3` types and throw an error if not.
#' @param x An object to check.
#' @param n,nrow,ncol The expected length, number of columns, or number of
#' rows of `x`.
#' @param ... Additional arguments passed to [`cli_abort()`][cli::cli_abort]
#' which forwards unmatched arguments to [`abort()`][rlang::abort].
#' @param allow_null Whether `x` is allowed to be `NULL`.
#' @inheritParams type-checks allow_na allow_null finite
#' @inheritParams rlang::args_error_context
#' @return `NULL` invisibly if the check passes, otherwise an error is thrown.
#' @details
#' These functions can be used with the [`bare()`] modifier to check if
#' an object is a bare S3 object (where the expected S3 type is the first
#' class in the class attribute of `x`), and the length modifiers
#' [`at_least()`], [`at_most()`], and [`in_range()`] to modify the behaviour
#' of the length checking `n`, `nrow`, and `ncol` arguments.
#' @name s3-type-checks
#' @family checks
#' @examples
#' x <- as.Date("2000-01-01")
#' check_date(x)
#' check_date(1L) |> try()
#'
#' class(x) <- c("my_date", class(x))
#' check_date(bare(x)) |> try()
#'
#' x <- x + 1:5
#' check_date(x, n = 3) |> try()
#' check_date(x, n = at_least(10)) |> try()
#' check_date(x, n = at_most(3)) |> try()
#' check_date(x, n = in_range(6, 10)) |> try()
#'
#' x <- data.frame(x = 1:3, y = 1:3)
#' check_tibble(x) |> try()
#'
#' class(x) <- c("my_tbl", "tbl_df", class(x))
#' check_tibble(x)
#' check_tibble(bare(x)) |> try()
#'
#' check_tibble(x, nrow = 2) |> try()
#' check_tibble(x, nrow = at_least(4)) |> try()
#' check_tibble(x, nrow = in_range(1, 2)) |> try()
#' check_tibble(x, ncol = 3) |> try()
#' check_tibble(x, ncol = at_most(1)) |> try()
#' check_tibble(x, ncol = in_range(3, 5)) |> try()
NULL

#' @rdname s3-type-checks
#' @export
check_date <- function(
  x,
  n = NULL,
  ...,
  allow_na = TRUE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  s3_vec_check(
    x,
    n,
    "Date",
    "a {.cls Date} vector",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )

  na_check(allow_na, x, n, arg, ..., call = call)
}

#' @rdname s3-type-checks
#' @export
check_posixct <- function(
  x,
  n = NULL,
  ...,
  allow_na = TRUE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  s3_vec_check(
    x,
    n,
    "POSIXct",
    "a {.cls POSIXct} vector",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )

  na_check(allow_na, x, n, arg, ..., call = call)
}

#' @rdname s3-type-checks
#' @export
check_posixlt <- function(
  x,
  n = NULL,
  ...,
  allow_na = TRUE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  s3_vec_check(
    x,
    n,
    "POSIXlt",
    "a {.cls POSIXlt} vector",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )

  na_check(allow_na, x, n, arg, ..., call = call)
}

#' @rdname s3-type-checks
#' @export
check_factor <- function(
  x,
  n = NULL,
  ...,
  finite = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  s3_vec_check(
    x,
    n,
    "factor",
    "a {.cls factor} vector",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )

  finite_check(finite, x, n, arg, ..., call = call)
}

#' @rdname s3-type-checks
#' @export
check_ordered <- function(
  x,
  n = NULL,
  ...,
  finite = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  s3_vec_check(
    x,
    n,
    "ordered",
    "a {.cls ordered} vector",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )

  finite_check(finite, x, n, arg, ..., call = call)
}

#' @rdname s3-type-checks
#' @export
check_vctr <- function(
  x,
  n = NULL,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  s3_vec_check(
    x,
    n,
    "vctrs_vctr",
    "a {.cls vctrs_vctr}",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

#' @rdname s3-type-checks
#' @export
check_list_of <- function(
  x,
  n = NULL,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  s3_vec_check(
    x,
    n,
    "vctrs_list_of",
    "a {.cls vctrs_list_of}",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

#' @rdname s3-type-checks
#' @export
check_data_frame <- function(
  x,
  nrow = NULL,
  ncol = NULL,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  s3_df_check(
    x,
    nrow,
    ncol,
    "data.frame",
    "a {.cls data.frame}",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

#' @rdname s3-type-checks
#' @export
check_tibble <- function(
  x,
  nrow = NULL,
  ncol = NULL,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  s3_df_check(
    x,
    nrow,
    ncol,
    "tbl_df",
    "a {.cls tbl_df}",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

#' @rdname s3-type-checks
#' @export
check_data_table <- function(
  x,
  nrow = NULL,
  ncol = NULL,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  s3_df_check(
    x,
    nrow,
    ncol,
    "data.table",
    "a {.cls data.table}",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

#' @rdname s3-type-checks
#' @export
check_tidytable <- function(
  x,
  nrow = NULL,
  ncol = NULL,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  s3_df_check(
    x,
    nrow,
    ncol,
    "tidytable",
    "a {.cls tidytable}",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}
