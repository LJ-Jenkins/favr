# Type checks

Check if inputs are expected types and throw an error if not.

## Usage

``` r
check_list(
  x,
  n = NULL,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_atomic(
  x,
  n = NULL,
  ...,
  allow_na = TRUE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_vector(
  x,
  n = NULL,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_integer(
  x,
  n = NULL,
  ...,
  allow_na = TRUE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_integerish(
  x,
  n = NULL,
  ...,
  finite = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_double(
  x,
  n = NULL,
  ...,
  finite = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_complex(
  x,
  n = NULL,
  ...,
  finite = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_character(
  x,
  n = NULL,
  ...,
  allow_na = TRUE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_logical(
  x,
  n = NULL,
  ...,
  allow_na = TRUE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_raw(
  x,
  n = NULL,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_bytes(
  x,
  n = NULL,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_null(x, ..., arg = caller_arg(x), call = caller_env())

check_numeric(
  x,
  n = NULL,
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

- n:

  The expected length of `x`.

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
attribute), and the length modifiers
[`at_least()`](https://lj-jenkins.github.io/favr/reference/modifiers.md),
[`at_most()`](https://lj-jenkins.github.io/favr/reference/modifiers.md),
and
[`in_range()`](https://lj-jenkins.github.io/favr/reference/modifiers.md)
to modify the behaviour of the length checking `n` argument.

## Note

`check_null()` cannot use
[`bare()`](https://lj-jenkins.github.io/favr/reference/modifiers.md)
since `NULL` cannot have a class attribute.

These check functions are wrappers of their corresponding
[rlang](https://rlang.r-lib.org/reference/type-predicates.html)
functions. The exception is `check_numeric()`, which uses
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
[`scalar-type-checks`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md),
[`scalar-value-checks`](https://lj-jenkins.github.io/favr/reference/scalar-value-checks.md),
[`walk-check`](https://lj-jenkins.github.io/favr/reference/walk-check.md)

## Examples

``` r
x <- c(1, 2, 3)

check_integer(x) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must be an <integer> vector, not a <double> vector.
check_integerish(x)
check_scalar_double(x) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must be a scalar <double>, but it is of length 3.
check_double(x, n = 2) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must be a <double> vector of length 2, not 3.
check_double(x, n = at_least(4)) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must be a <double> vector of at least length 4, but it is of length
#> 3.
check_double(x, n = at_most(2)) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must be a <double> vector of at most length 2, but it is of length
#> 3.
check_double(x, n = in_range(1, 2)) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must be a <double> vector of a length between 1 and 2, but it is of
#> length 3.

check_integer(bare(factor(1))) |> try()
#> Error in eval(expr, envir) : 
#>   `factor(1)` must be a bare <integer>, but it is of class <factor>.

check_double(c(1L, NA), allow_na = FALSE) |> try()
#> Error in check_types_impl(is_double, "a {.cls double} vector", x, n = n,  : 
#>   formal argument "allow_na" matched by multiple actual arguments
check_double(c(1.5, NA), finite = TRUE) |> try()
#> Error in eval(expr, envir) : 
#>   `c(1.5, NA)` must not contain non-finite values.

check_double(NULL, allow_null = TRUE)

# NULL list elements are not considered NULL
check_list(list(NULL), allow_null = TRUE) |> try()
```
