# Scalar type checks

Check if inputs are scalars of an expected type and throw an error if
not.

## Usage

``` r
check_scalar_list(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_scalar_atomic(
  x,
  ...,
  allow_na = TRUE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_scalar_vector(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_scalar_integer(
  x,
  ...,
  allow_na = TRUE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_scalar_integerish(
  x,
  ...,
  finite = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_scalar_double(
  x,
  ...,
  finite = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_scalar_complex(
  x,
  ...,
  finite = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_scalar_character(
  x,
  ...,
  allow_na = TRUE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_scalar_logical(
  x,
  ...,
  allow_na = TRUE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_scalar_raw(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_scalar_bytes(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_scalar_numeric(
  x,
  ...,
  finite = FALSE,
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

- allow_na:

  Whether `x` is allowed to contain `NA` values.

- finite:

  Whether `x` is required to contain only finite values (i.e. no `NA`,
  `Inf`, `-Inf`, or `NaN`).

## Value

`NULL` invisibly if the check passes, otherwise an error is thrown.

## Details

These functions can be used with the
[`bare()`](https://lj-jenkins.github.io/favr/reference/modifiers.md)
modifier to check if an object is a bare R object (i.e. has no class
attribute).

## Note

To handle empty strings (`""`) use
[`check_string()`](https://lj-jenkins.github.io/favr/reference/scalar-value-checks.md)
instead of `check_scalar_character()`.

These check functions are wrappers of their corresponding
[rlang](https://rlang.r-lib.org/reference/scalar-type-predicates.html)
functions. The exception is `check_scalar_numeric()`, which uses
[`is.numeric()`](https://rdrr.io/r/base/numeric.html).

## See also

Other checks:
[`array-type-checks`](https://lj-jenkins.github.io/favr/reference/array-type-checks.md),
[`forbidden-value-checks`](https://lj-jenkins.github.io/favr/reference/forbidden-value-checks.md),
[`inheritance-checks`](https://lj-jenkins.github.io/favr/reference/inheritance-checks.md),
[`oop-checks`](https://lj-jenkins.github.io/favr/reference/oop-checks.md),
[`path-checks`](https://lj-jenkins.github.io/favr/reference/path-checks.md),
[`property-checks`](https://lj-jenkins.github.io/favr/reference/property-checks.md),
[`s3-type-checks`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md),
[`scalar-value-checks`](https://lj-jenkins.github.io/favr/reference/scalar-value-checks.md),
[`type-checks`](https://lj-jenkins.github.io/favr/reference/type-checks.md),
[`walk-check`](https://lj-jenkins.github.io/favr/reference/walk-check.md)

## Examples

``` r
x <- 1L
check_scalar_integer(x)
check_scalar_double(x) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must be a scalar <double>, not the number 1.

check_scalar_list(list(list()))
check_scalar_list(list(1, 2)) |> try()
#> Error in eval(expr, envir) : 
#>   `list(1, 2)` must be a scalar <list>, but it is of length 2.

check_scalar_character(NA_character_, allow_na = FALSE) |> try()
#> Error in eval(expr, envir) : 
#>   `NA_character_` must not be NA.
check_scalar_double(Inf, finite = TRUE) |> try()
#> Error in eval(expr, envir) : 
#>   `Inf` must be a finite value, not Inf.

check_scalar_logical(NULL, allow_null = TRUE)

x <- 1.0
check_scalar_integerish(x)
```
