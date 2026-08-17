# Apply a predicate check to each element of a vector

Apply a predicate check function to each element of a vector and throw
an error if any element fails the check.

## Usage

``` r
walk_check(.x, .f, ..., call = caller_env())
```

## Arguments

- .x:

  A list or atomic vector.

- .f:

  A function or formula to apply to each element of `.x`. Must return a
  logical vector of [all](https://rdrr.io/r/base/all.html) `TRUE` for no
  error to occur. Non-`logical` and `NA` values will trigger an error.

- ...:

  Additional arguments passed to
  [cli_abort()](https://cli.r-lib.org/reference/cli_abort.html) which
  forwards unmatched arguments to
  [abort()](https://rlang.r-lib.org/reference/abort.html).

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the call argument of
  [abort()](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

`.x` invisibly if all checks pass, otherwise an error is thrown.

## Details

If you wish to use a function for `.f` that errors, pass contextual
information to that function directly (e.g., using a shorthand anonymous
function), as the error will be thrown from that function's context and
won't have access to `...` and `call` from the calling function.

## See also

Other checks:
[`array-type-checks`](https://lj-jenkins.github.io/favr/reference/array-type-checks.md),
[`forbidden-value-checks`](https://lj-jenkins.github.io/favr/reference/forbidden-value-checks.md),
[`inheritance-checks`](https://lj-jenkins.github.io/favr/reference/inheritance-checks.md),
[`path-checks`](https://lj-jenkins.github.io/favr/reference/path-checks.md),
[`s3-type-checks`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md),
[`scalar-type-checks`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md),
[`scalar-value-checks`](https://lj-jenkins.github.io/favr/reference/scalar-value-checks.md),
[`type-checks`](https://lj-jenkins.github.io/favr/reference/type-checks.md)

## Examples

``` r
x <- list(1, 2, "a")
walk_check(x, is.atomic)
walk_check(x, ~ length(.x) == 1L)
walk_check(x, is.numeric) |> try()
#> Error in eval(expr, envir) : 
#>   Check result for `.x[[3]]` is not TRUE.
walk_check(x, \(el) nchar(el) == 1L)

# Named elements are shown in the error.
x <- list(a = 1, b = 2, c = "a")
walk_check(x, is.numeric) |> try()
#> Error in eval(expr, envir) : 
#>   Check result for `.x[['c']]` (index: 3) is not TRUE.
```
