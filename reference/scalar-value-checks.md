# Scalar value checks

Check if inputs are expected scalar values and throw an error if not.

## Usage

``` r
check_true(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_false(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_bool(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_string(
  x,
  ...,
  string = NULL,
  allow_empty = TRUE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)
```

## Arguments

- x:

  An object to check.

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

- string:

  A character vector of allowed values for `x`. If `NULL`, the value is
  not checked. The check passes if `x` is **any** of the values in
  `string`.

- allow_empty:

  Whether `x` is allowed to be an empty string (i.e. when `FALSE` `""`
  is not allowed).

## Value

`NULL` invisibly if the check passes, otherwise an error is thrown.

## Note

The [favr
modifiers](https://lj-jenkins.github.io/favr/reference/modifiers.md)
cannot be used with these functions.

## See also

Other checks:
[`array-type-checks`](https://lj-jenkins.github.io/favr/reference/array-type-checks.md),
[`forbidden-value-checks`](https://lj-jenkins.github.io/favr/reference/forbidden-value-checks.md),
[`inheritance-checks`](https://lj-jenkins.github.io/favr/reference/inheritance-checks.md),
[`path-checks`](https://lj-jenkins.github.io/favr/reference/path-checks.md),
[`property-checks`](https://lj-jenkins.github.io/favr/reference/property-checks.md),
[`s3-type-checks`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md),
[`scalar-type-checks`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md),
[`type-checks`](https://lj-jenkins.github.io/favr/reference/type-checks.md),
[`walk-check`](https://lj-jenkins.github.io/favr/reference/walk-check.md)

## Examples

``` r
x <- TRUE
check_true(x)
check_false(x) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must be a single FALSE, not TRUE.

check_bool(NA) |> try()
#> Error in eval(expr, envir) : 
#>   `NA` must be a single TRUE or FALSE, not NA.
check_bool(NULL, allow_null = TRUE)

x <- "a"
check_string(x)
check_string(x, string = c("a", "b"))
check_string(x, string = c("b", "c")) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must be one of "b" or "c".
check_string("", allow_empty = FALSE) |> try()
#> Error in eval(expr, envir) : `""` must not be an empty string.
```
