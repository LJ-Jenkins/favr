# File and directory existence checks

Check if inputs are existing directories or files and throw an error if
not.

## Usage

``` r
check_dir(x, ..., arg = caller_arg(x), call = caller_env())

check_file(
  x,
  ...,
  ext = NULL,
  case = TRUE,
  x_arg = caller_arg(x),
  ext_arg = caller_arg(ext),
  call = caller_env()
)

check_ext(
  x,
  ext,
  ...,
  case = TRUE,
  x_arg = caller_arg(x),
  ext_arg = caller_arg(ext),
  call = caller_env()
)
```

## Arguments

- x:

  A path to check.

- ...:

  Additional arguments passed to
  [`cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html) which
  forwards unmatched arguments to
  [`abort()`](https://rlang.r-lib.org/reference/abort.html).

- arg, x_arg, ext_arg:

  An argument name as a string. This argument will be mentioned in error
  messages as the input that is at the origin of a problem.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html) for
  more information.

- ext:

  A character vector of file extensions to check for.

- case:

  A logical value indicating if the extension check should be
  case-sensitive. If `FALSE`, the check will be case-insensitive.

## Value

`NULL` invisibly if the check passes, otherwise an error is thrown.

## Note

The checking of extensions is done simply using
[`endsWith()`](https://rdrr.io/r/base/startsWith.html).

## See also

Other checks:
[`array-type-checks`](https://lj-jenkins.github.io/favr/reference/array-type-checks.md),
[`forbidden-value-checks`](https://lj-jenkins.github.io/favr/reference/forbidden-value-checks.md),
[`inheritance-checks`](https://lj-jenkins.github.io/favr/reference/inheritance-checks.md),
[`oop-checks`](https://lj-jenkins.github.io/favr/reference/oop-checks.md),
[`property-checks`](https://lj-jenkins.github.io/favr/reference/property-checks.md),
[`s3-type-checks`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md),
[`scalar-type-checks`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md),
[`scalar-value-checks`](https://lj-jenkins.github.io/favr/reference/scalar-value-checks.md),
[`type-checks`](https://lj-jenkins.github.io/favr/reference/type-checks.md),
[`walk-check`](https://lj-jenkins.github.io/favr/reference/walk-check.md)

## Examples

``` r
x <- file.path(R.home(), "library", "stats")

check_dir(x)
check_file(x) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must be an existing file, but it is a directory.
#> ℹ Path provided: /opt/R/4.6.1/lib/R/library/stats.

x <- file.path(x, "DESCRIPTION")

check_file(x)
check_dir(x) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must be an existing directory, but it is a file.
#> ℹ Path provided: /opt/R/4.6.1/lib/R/library/stats/DESCRIPTION.
check_file(x, ext = c(".csv", ".xlsx")) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must have extension ".csv" or ".xlsx".
```
