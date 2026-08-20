# Array type checks

Check if inputs are expected types and throw an error if not.

## Usage

``` r
check_array(
  x,
  n = NULL,
  nrow = NULL,
  ncol = NULL,
  ...,
  finite = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_matrix(
  x,
  n = NULL,
  nrow = NULL,
  ncol = NULL,
  ...,
  finite = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_table(
  x,
  n = NULL,
  nrow = NULL,
  ncol = NULL,
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

- n, nrow, ncol:

  The expected length, number of columns, or number of rows of `x`.

- ...:

  Additional arguments passed to
  [`cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html) which
  forwards unmatched arguments to
  [`abort()`](https://rlang.r-lib.org/reference/abort.html).

- finite:

  Whether `x` is required to contain only finite values (i.e. no `NA`,
  `Inf`, `-Inf`, or `NaN`).

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

These functions can be used with the
[`bare()`](https://lj-jenkins.github.io/favr/reference/modifiers.md)
modifier to check if an object is a bare R object (i.e. has no class
attribute), and the length modifiers
[`at_least()`](https://lj-jenkins.github.io/favr/reference/modifiers.md),
[`at_most()`](https://lj-jenkins.github.io/favr/reference/modifiers.md),
and
[`in_range()`](https://lj-jenkins.github.io/favr/reference/modifiers.md)
to modify the behaviour of the length checking `n`, `nrow`, and `ncol`
arguments.

Note that the
[`bare()`](https://lj-jenkins.github.io/favr/reference/modifiers.md)
modifier uses [`is.object()`](https://rdrr.io/r/base/is.object.html) for
`check_array()` and `check_matrix()`, but uses the S3-style check for
`check_table()`, which checks if `"table"` is the first class in the
class vector.

## Note

These check functions are wrappers of their corresponding base functions
[`is.array()`](https://rdrr.io/r/base/array.html),
[`is.matrix()`](https://rdrr.io/r/base/matrix.html) and
[`is.table()`](https://rdrr.io/r/base/table.html).

## See also

Other checks:
[`forbidden-value-checks`](https://lj-jenkins.github.io/favr/reference/forbidden-value-checks.md),
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
a <- array(1:12, dim = c(3, 4))
check_array(a)
check_array(1:12) |> try()
#> Error in eval(expr, envir) : 
#>   `1:12` must be an <array>, not an <integer> vector.

m <- matrix(1:12, nrow = 3)
check_matrix(m)
check_matrix(1:12) |> try()
#> Error in eval(expr, envir) : 
#>   `1:12` must be a <matrix>, not an <integer> vector.

t <- table(c("a", "b", "a"))
check_table(t)
check_table(1:12) |> try()
#> Error in eval(expr, envir) : 
#>   `1:12` must be a <table>, not an <integer> vector.

class(m) <- c("my_matrix", class(m))
check_matrix(bare(m)) |> try()
#> Error in eval(expr, envir) : 
#>   `m` must be a bare <matrix>, but it is of class
#> <my_matrix/matrix/array>.

check_array(a, n = 10) |> try()
#> Error in eval(expr, envir) : 
#>   `a` must be an <array> of length 10, not 12.
check_array(a, n = at_least(10))

check_matrix(m, ncol = at_most(3)) |> try()
#> Error in eval(expr, envir) : 
#>   `m` must be a <matrix> with at most 3 columns, but it has 4.
check_matrix(m, nrow = in_range(1, 10))
```
