# Check if an object is of a specific object-oriented programming type

Check that an object is an `S3`, `S4`, `S7`, or `R6` object.

## Usage

``` r
check_s3(x, ..., allow_null = FALSE, arg = caller_arg(x), call = caller_env())

check_s4(x, ..., allow_null = FALSE, arg = caller_arg(x), call = caller_env())

check_s7(x, ..., allow_null = FALSE, arg = caller_arg(x), call = caller_env())

check_r6(x, ..., allow_null = FALSE, arg = caller_arg(x), call = caller_env())
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

## Value

`NULL` invisibly if the check passes or an error if it fails.

## Details

`S3` checks are performed by checking if the object has a class
attribute with [`is.object()`](https://rdrr.io/r/base/is.object.html)
and is not an `S4` object.

`S4` checks are performed using
[`isS4()`](https://rdrr.io/r/base/isS4.html).

`S7` and `R6` checks are performed by checking for the inheritance of
the `S7_object` and `R6` classes, respectively.

## See also

Other checks:
[`array-type-checks`](https://lj-jenkins.github.io/favr/reference/array-type-checks.md),
[`forbidden-value-checks`](https://lj-jenkins.github.io/favr/reference/forbidden-value-checks.md),
[`inheritance-checks`](https://lj-jenkins.github.io/favr/reference/inheritance-checks.md),
[`path-checks`](https://lj-jenkins.github.io/favr/reference/path-checks.md),
[`property-checks`](https://lj-jenkins.github.io/favr/reference/property-checks.md),
[`s3-type-checks`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md),
[`scalar-type-checks`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md),
[`scalar-value-checks`](https://lj-jenkins.github.io/favr/reference/scalar-value-checks.md),
[`type-checks`](https://lj-jenkins.github.io/favr/reference/type-checks.md),
[`walk-check`](https://lj-jenkins.github.io/favr/reference/walk-check.md)

## Examples

``` r
check_s3(factor("a"))
check_s3(1:3) |> try()
#> Error in eval(expr, envir) : 
#>   `1:3` must be an <S3> object, not <integer>.

setClass("Person",
  slots = c(name = "character", age = "numeric")
)
x <- new("Person", name = "John", age = 30)

check_s4(x)
check_s4(factor("a")) |> try()
#> Error in eval(expr, envir) : 
#>   `factor("a")` must be an <S4> object, not <factor>.

# trivial examples of inheritance checks for S7 and R6 objects
x <- structure(list(), class = "S7_object")
check_s7(x)
check_s7(factor("a")) |> try()
#> Error in eval(expr, envir) : 
#>   `factor("a")` must inherit from <S7_object>, but is class <factor>.

x <- structure(list(), class = "R6")
check_r6(x)
check_r6(factor("a")) |> try()
#> Error in eval(expr, envir) : 
#>   `factor("a")` must inherit from <R6>, but is class <factor>.
```
