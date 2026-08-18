# Forbidden value checks

Check if inputs contain forbidden values and error if so.

## Usage

``` r
check_no_na(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_finite(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_unique(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_nzchar(
  x,
  ...,
  allow_all_ws = TRUE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)
```

## Arguments

- x:

  An **R** object.

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

- allow_all_ws:

  Whether `x` is allowed to contain elements that are all whitespace.

## Value

`NULL` invisibly if the check passes, otherwise an error is thrown.

## Details

`NA` checks are done with [`anyNA()`](https://rdrr.io/r/base/NA.html);

finite checks are done with [`any()`](https://rdrr.io/r/base/any.html)
and [`is.finite()`](https://rdrr.io/r/base/is.finite.html);

unique checks are done with
[`anyDuplicated()`](https://rdrr.io/r/base/duplicated.html);

zero chr checks are done with [`any()`](https://rdrr.io/r/base/any.html)
and [`nzchar()`](https://rdrr.io/r/base/nchar.html);

If `allow_all_ws = FALSE` then whitespace elements are identified using
`grepl("\\s+", x)`.

Input types are not checked to be of expected types, they are passed 'as
is' to the functions that do the checking. The only exception is for
`NULL` inputs, which error if `allow_null = FALSE`.

## Note

The [favr
modifiers](https://lj-jenkins.github.io/favr/reference/modifiers.md)
cannot be used with these functions.

`NA_character_` is not considered zero chr nor all whitespace.

## See also

Other checks:
[`array-type-checks`](https://lj-jenkins.github.io/favr/reference/array-type-checks.md),
[`inheritance-checks`](https://lj-jenkins.github.io/favr/reference/inheritance-checks.md),
[`oop-checks`](https://lj-jenkins.github.io/favr/reference/oop-checks.md),
[`path-checks`](https://lj-jenkins.github.io/favr/reference/path-checks.md),
[`property-checks`](https://lj-jenkins.github.io/favr/reference/property-checks.md),
[`s3-type-checks`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md),
[`scalar-type-checks`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md),
[`scalar-value-checks`](https://lj-jenkins.github.io/favr/reference/scalar-value-checks.md),
[`type-checks`](https://lj-jenkins.github.io/favr/reference/type-checks.md),
[`walk-check`](https://lj-jenkins.github.io/favr/reference/walk-check.md)

## Examples

``` r
x <- c(1, 2, NA)
check_no_na(x) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must not contain NA values.

x <- c(1, 2, Inf)
check_finite(x) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must not contain non-finite values.

x <- c(1, 2, 3, 1)
check_unique(x) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must have unique elements. Duplicates: 1.

x <- c("a", "b", "")
check_nzchar(x) |> try()
#> Error in eval(expr, envir) : `x` must not contain empty strings.

x <- c("a", "b", " ")
check_nzchar(x, allow_all_ws = FALSE) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must not contain all whitespace elements.
```
