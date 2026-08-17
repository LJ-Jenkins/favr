# Check class inheritance of an object

Check that an object inherits from a specific class (or classes) and
throw an error if not.

## Usage

``` r
check_inherits(
  x,
  class,
  match = c("any", "exact", "all"),
  ...,
  arg = caller_arg(x),
  call = caller_env()
)

check_class(x, class, ..., arg = caller_arg(x), call = caller_env())
```

## Arguments

- x:

  An object to check.

- class:

  Character vector of class names to check against.

- match:

  The behaviour to use for inheritance checking. See Details.

- ...:

  Additional arguments passed to
  [cli_abort()](https://cli.r-lib.org/reference/cli_abort.html) which
  forwards unmatched arguments to
  [abort()](https://rlang.r-lib.org/reference/abort.html).

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

`NULL` invisibly if the check passes or an error if it fails.

## Details

The `match` argument specifies how to check the inheritance:

- `"any"`: the class vector of `x` must have at least one element in
  common with `class`.

- `"exact"`: the class vector of `x` must be identical to `class`.

- `"all"`: the class vector of `x` must contain all elements of `class`
  in the supplied order.

`check_class()` is a utility wrapper around `check_inherits()` with
`match = "exact"`.

## Note

These check functions are wrappers of their corresponding
[rlang](https://rlang.r-lib.org/reference/inherits_any.html)
counterparts.

## See also

Other checks:
[`array-type-checks`](https://lj-jenkins.github.io/favr/reference/array-type-checks.md),
[`forbidden-value-checks`](https://lj-jenkins.github.io/favr/reference/forbidden-value-checks.md),
[`path-checks`](https://lj-jenkins.github.io/favr/reference/path-checks.md),
[`s3-type-checks`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md),
[`scalar-type-checks`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md),
[`scalar-value-checks`](https://lj-jenkins.github.io/favr/reference/scalar-value-checks.md),
[`type-checks`](https://lj-jenkins.github.io/favr/reference/type-checks.md),
[`walk-check`](https://lj-jenkins.github.io/favr/reference/walk-check.md)

## Examples

``` r
# Default behaviour is to check for any inheritance.
x <- structure(1, class = c("a", "b", "c"))
check_inherits(x, c("x", "b", "y"))
check_inherits(x, c("x", "y")) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must inherit from any of <x/y>, but is class <a/b/c>.

# `match = "exact"` checks for exact match of the class vector.
x <- structure(1, class = c("a", "b", "c"))
check_inherits(x, c("a", "b", "c"), match = "exact")
check_inherits(x, c("a", "b")) |> try()

# check_class() is a utility wrapper with match = "exact".
check_class(x, c("a", "b", "c"))
check_class(x, c("a", "b")) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must be class <a/b>, but is class <a/b/c>.

# `match = "all"` checks that inheritance is from all
# of the classes in the supplied order.
x <- structure(1, class = c("a", "b", "c", "d", "e"))
check_inherits(x, c("b", "d"), match = "all")
check_inherits(x, c("d", "b"), match = "all") |> try()
#> Error in eval(expr, envir) : 
#>   `x` must inherit from all of class <d/b> in order, but is class
#> <a/b/c/d/e>.
```
