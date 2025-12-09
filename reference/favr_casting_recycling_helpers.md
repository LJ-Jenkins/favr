# favr casting and recycling helpers

These functions signal to favr functions to undergo casting, lossy
casting, and/or recycling. Each can only be used wihtin calls to
specific favr functions and will error if used outside them.
Specifically:

- `lossy()`: used within
  [`cast_if_not()`](https://lj-jenkins.github.io/favr/reference/cast_if_not.md)
  for lossy casting.

- `cast()`: used within
  [`enforce()`](https://lj-jenkins.github.io/favr/reference/enforce.md)
  and
  [`schema()`](https://lj-jenkins.github.io/favr/reference/schema.md)
  for casting.

- `recycle()`: used within
  [`enforce()`](https://lj-jenkins.github.io/favr/reference/enforce.md)
  and
  [`schema()`](https://lj-jenkins.github.io/favr/reference/schema.md)
  for recycling.

- `coerce()`: used within
  [`enforce()`](https://lj-jenkins.github.io/favr/reference/enforce.md)
  and
  [`schema()`](https://lj-jenkins.github.io/favr/reference/schema.md)
  for casting and recycling.

## Usage

``` r
lossy(x)

cast(x, lossy = FALSE)

recycle(x)

coerce(type = NULL, size = NULL, lossy = FALSE)
```

## Arguments

- x:

  input to be lossily casted for `lossy()`, object of type to cast to
  for `cast()`, or scalar integerish value to recycle to for
  `recycle()`.

- lossy:

  logical, `TRUE` or `FALSE`.

- type:

  object of type to cast to for `coerce()`.

- size:

  scalar integerish value to recycle to.

## Value

No return value, called for side effects only. Will error if called
outside of a favr calling context (see Description and Examples).

## Details

These functions add attributes and/or a class to their inputs that
signal transformations to occur within the favr caller.

## Examples

``` r
try(cast(10)) # errors outside of favr calling context
#> Error in eval(expr, envir) : Caused by error in `cast()`.
#> `cast()` must be used within `enforce()` or `schema()` calls.

x <- 1.5
cast_if_not(x = lossy(integer()))
class(x) # integer
#> [1] "integer"

enforce(x ~ list(cast(double()), recycle(5)))
class(x) # numeric
#> [1] "numeric"
length(x) # 5
#> [1] 5

x <- 1.5
enforce(x ~ coerce(type = integer(), size = 5, lossy = TRUE))
class(x) # integer
#> [1] "integer"
length(x) # 5
#> [1] 5
```
