# Modify the behaviour of type checking functions

Modify the type-checking, or length-checking behaviour of
[favr](https://lj-jenkins.github.io/favr/reference/favr-package.md) type
checking functions.

## Usage

``` r
bare(x, arg = caller_arg(x))

at_least(n, arg = caller_arg(n))

at_most(n, arg = caller_arg(n))

in_range(
  n_min,
  n_max,
  arg_min = caller_arg(n_min),
  arg_max = caller_arg(n_max)
)
```

## Arguments

- x:

  An object to modify the check behaviour for.

- arg, arg_min, arg_max:

  An argument name as a string. This argument will be mentioned in error
  messages as the input that is at the origin of a problem.

- n, n_min, n_max:

  Single numeric value that is castable to an integer. Must be zero or
  positive.

## Value

A list of class `favr_modifier` with named elements `obj`, `bare` and
`arg` for `bare()`, and `at_least` and/or `at_most` for the length
modifiers.

## Details

Use `bare()` to check if a given object is a bare R object (no class
attribute, see [`is.object()`](https://rdrr.io/r/base/is.object.html)),
throwing an error if it is not and passing the object on to the check if
it is.

For `S3` type checks, `bare()` checks that the object has the expected
`S3` type as the **first** element of the class vector.

To modify the behaviour of length checking arguments `n`, `nrow`, and
`ncol` (example described for `n`):

- `at_least(n)` means the object must be at least length (`>=`) `n`.

- `at_most(n)` means the object must be at most length (`<=`) `n`.

- `in_range(n_min, n_max)` means the object length must be within the
  range of (`>=`) `n_min` and (`<=`) `n_max`.

## See also

[type-checks](https://lj-jenkins.github.io/favr/reference/type-checks.md),
[scalar-type-checks](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md),
[s3-type-checks](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md),
[property-checks](https://lj-jenkins.github.io/favr/reference/property-checks.md)
and
[s3-check-builders](https://lj-jenkins.github.io/favr/reference/s3-check-builders.md)
for the functions that these modifiers can be used with.

## Examples

``` r
bare(1)
#> $obj
#> [1] 1
#> 
#> $bare
#> [1] TRUE
#> 
#> $arg
#> [1] "1"
#> 
#> attr(,"class")
#> [1] "favr_bare"     "favr_modifier"
at_least(1)
#> $at_least
#> [1] 1
#> 
#> attr(,"class")
#> [1] "favr_at_least" "favr_modifier"
at_most(1)
#> $at_most
#> [1] 1
#> 
#> attr(,"class")
#> [1] "favr_at_most"  "favr_modifier"
in_range(1, 2)
#> $at_least
#> [1] 1
#> 
#> $at_most
#> [1] 2
#> 
#> attr(,"class")
#> [1] "favr_in_range" "favr_modifier"

at_least(1.5) |> try()
#> Error in at_least(1.5) : 
#>   Can't convert from `1.5` <double> to `at_least 'n'` <integer> due to loss of precision.
#> • Locations: 1

check_integer(bare(factor(1))) |> try()
#> Error in eval(expr, envir) : 
#>   `factor(1)` must be a bare <integer>, but it is of class <factor>.
check_integer(1:5, n = at_least(10)) |> try()
#> Error in eval(expr, envir) : 
#>   `1:5` must be an <integer> vector of at least length 10, but it is of
#> length 5.
check_integer(1:5, n = at_most(3)) |> try()
#> Error in eval(expr, envir) : 
#>   `1:5` must be an <integer> vector of at most length 3, but it is of
#> length 5.
check_integer(1:5, n = in_range(2, 4)) |> try()
#> Error in eval(expr, envir) : 
#>   `1:5` must be an <integer> vector of a length between 2 and 4, but it is
#> of length 5.

x <- as.Date("2000-01-01")
class(x) <- c("my_date", class(x))
check_date(bare(x)) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must be a bare <Date>, but it is of class <my_date>.
```
