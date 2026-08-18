# Object property checks

Check if inputs have certain properties and error if not.

## Usage

``` r
check_length(
  x,
  n,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_nrow(
  x,
  nrow,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_ncol(
  x,
  ncol,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_size(
  x,
  n,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_non_empty(x, ..., arg = caller_arg(x), call = caller_env())

check_named(
  x,
  ...,
  unique = FALSE,
  allow_empty = TRUE,
  arg = caller_arg(x),
  call = caller_env()
)
```

## Arguments

- x:

  An **R** object.

- n, nrow, ncol:

  The expected length/size, number of columns, or number of rows of `x`.

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

- unique:

  Whether `x` is required to have unique names.

- allow_empty:

  Whether `x` is allowed to have empty names (`""`).

## Value

`NULL` invisibly if the check passes, otherwise an error is thrown.

## Details

Input types are not checked to be of expected types, they are passed 'as
is' to the functions that do the property checking. The only exception
is for `NULL` inputs, which error if `allow_null = FALSE`.

`check_size()` uses
[`vec_size()`](https://vctrs.r-lib.org/reference/vec_size.html) to
determine the size of `x`, as opposed to
[`length()`](https://rdrr.io/r/base/length.html).

## See also

Other checks:
[`array-type-checks`](https://lj-jenkins.github.io/favr/reference/array-type-checks.md),
[`forbidden-value-checks`](https://lj-jenkins.github.io/favr/reference/forbidden-value-checks.md),
[`inheritance-checks`](https://lj-jenkins.github.io/favr/reference/inheritance-checks.md),
[`path-checks`](https://lj-jenkins.github.io/favr/reference/path-checks.md),
[`s3-type-checks`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md),
[`scalar-type-checks`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md),
[`scalar-value-checks`](https://lj-jenkins.github.io/favr/reference/scalar-value-checks.md),
[`type-checks`](https://lj-jenkins.github.io/favr/reference/type-checks.md),
[`walk-check`](https://lj-jenkins.github.io/favr/reference/walk-check.md)

## Examples

``` r
x <- c(1, 2, NA)
check_length(x, 4) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must be of length 4, not 3.
check_size(x, 4) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must be of size 4, not 3.

# length modifiers can be used
check_length(x, at_most(2)) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must be of at most length 2, but it is of length 3.

x <- data.frame(x = 1)
check_nrow(x, 2) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must have 2 rows, not 1.
check_ncol(x, in_range(2, 4)) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must have 2 to 4 columns, but it has 1.
check_size(x, at_least(2)) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must be of at least size 2, but it is of size 1.

x <- numeric(0)
check_non_empty(x) |> try()
#> Error in eval(expr, envir) : `x` must not be empty.
check_non_empty(NULL) |> try()
#> Error in eval(expr, envir) : `NULL` must not be empty.

x <- c(1, 2, 3)
check_named(x) |> try()
#> Error in eval(expr, envir) : `x` must be named.
names(x) <- c("a", "b", "a")
check_named(x, unique = TRUE) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must have unique names. Duplicates: "a".
names(x) <- c("a", "b", "")
check_named(x, allow_empty = FALSE) |> try()
#> Error in eval(expr, envir) : `x` must not contain empty names.

check_length(NULL, 2, allow_null = TRUE)
```
