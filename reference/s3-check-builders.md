# S3 check builders

Check builders for S3 types. These functions can be used to create
custom S3 type checks in the style of favr.

## Usage

``` r
s3_vec_check(
  x,
  n,
  type,
  type_msg = paste0("a {.cls ", type, "}"),
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

s3_df_check(
  x,
  nrow,
  ncol,
  type,
  type_msg = paste0("a {.cls ", type, "}"),
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)
```

## Arguments

- x:

  An object to check.

- n, nrow, ncol:

  The expected length, number of columns, or number of rows of `x`.

- type:

  The expected S3 type of `x`.

- type_msg:

  A message describing the expected S3 type of `x` for use in error
  messages not relating to inheritance, optionally with
  [cli](https://cli.r-lib.org/reference/cli.html) formatting. See
  details.

- ...:

  Additional arguments passed to
  [cli_abort()](https://cli.r-lib.org/reference/cli_abort.html) which
  forwards unmatched arguments to
  [abort()](https://rlang.r-lib.org/reference/abort.html).

- allow_null:

  Whether `x` is allowed to be `NULL`.

- arg:

  An argument name as a string. This argument will be mentioned in error
  messages as the input that is at the origin of a problem.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

`NULL` invisibly if the check passes, otherwise an error is thrown.

## Details

Inputs are passed to
[`check_inherits()`](https://lj-jenkins.github.io/favr/reference/inheritance-checks.md)
to check that `x` inherits from the expected S3 type. This means that
error messages about inheritance will always show the expected S3 type
in the cli format of `{.cls <expected_s3_type>}`.

The `type_msg` argument is used to customise the error message when a
different check fails (e.g., length), where the grammar may require
different phrasing. For example, the default value is
`"a {.cls <expected_s3_type>}"`, but many favr functions use
`"a {.cls <expected_s3_type>} vector"`. Also consider where 'an' is more
appropriate than 'a'.

These functions can be used with the
[`bare()`](https://lj-jenkins.github.io/favr/reference/modifiers.md)
modifier to check if an object is a bare S3 object (where the expected
S3 type is the first class in the class attribute of `x`), and the
length modifiers
[`at_least()`](https://lj-jenkins.github.io/favr/reference/modifiers.md),
[`at_most()`](https://lj-jenkins.github.io/favr/reference/modifiers.md),
and
[`in_range()`](https://lj-jenkins.github.io/favr/reference/modifiers.md)
to modify the behaviour of the length checking `n`, `nrow`, and `ncol`
arguments.

## Note

Although named `_vec` and `_df`, these functions could be used to check
any S3 type, not just vectors and data frames. Their names are intended
to indicate the expected behaviour of the check - for types that would
use the length or dimension checking arguments.

## Examples

``` r
# Create a custom type check for a hypothetical "my_class" S3 class
check_my_class <- function(
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
    type = "my_class",
    type_msg = "a {.cls my_class} vector",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

# inheritance errors use 'type'
check_my_class(1L) |> try()
#> Error in "fun(..., .envir = .envir)" : 
#>   ! Could not evaluate cli `{}` expression: `arg`.
#> Caused by error in `caller_arg(x)`:
#> ! could not find function "caller_arg"

x <- structure(1:3, class = "my_class")
check_my_class(x)
check_my_class(NULL, allow_null = TRUE)

# other errors use 'type_msg'
check_my_class(x, n = 2) |> try()
#> Error in "fun(..., .envir = .envir)" : 
#>   ! Could not evaluate cli `{}` expression: `arg`.
#> Caused by error in `caller_arg(x)`:
#> ! could not find function "caller_arg"
check_my_class(x, n = at_least(4)) |> try()
#> Error in "fun(..., .envir = .envir)" : 
#>   ! Could not evaluate cli `{}` expression: `arg`.
#> Caused by error in `caller_arg(x)`:
#> ! could not find function "caller_arg"
check_my_class(x, n = at_most(2)) |> try()
#> Error in "fun(..., .envir = .envir)" : 
#>   ! Could not evaluate cli `{}` expression: `arg`.
#> Caused by error in `caller_arg(x)`:
#> ! could not find function "caller_arg"
check_my_class(x, n = in_range(1, 2)) |> try()
#> Error in "fun(..., .envir = .envir)" : 
#>   ! Could not evaluate cli `{}` expression: `arg`.
#> Caused by error in `caller_arg(x)`:
#> ! could not find function "caller_arg"

class(x) <- c("another_class", class(x))
check_my_class(bare(x)) |> try()
#> Error in caller_env() : could not find function "caller_env"
```
