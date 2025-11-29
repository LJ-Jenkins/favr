# Is object named?

Wrappers around rlang predicates that allow multiple objects to be
passed. The following documentation is adapted from the rlang
documentation:

- `are_named()` is a scalar predicate that checks that objects in `...`
  have a `names` attribute and that none of the names are missing or
  empty (`NA` or `""`).

- `are_named2()` is like `are_named()` but always returns `TRUE` for
  empty vectors, even those that don't have a `names` attribute. In
  other words, it tests for the property that each element of a vector
  is named. `are_named2()` composes well with `names2()` whereas
  `are_named()` composes with
  [`names()`](https://rdrr.io/r/base/names.html).

- `have_names()` is a vectorised variant.

## Usage

``` r
are_named(..., .all = FALSE)

are_named2(..., .all = FALSE)

have_names(..., .all = FALSE)
```

## Arguments

- ...:

  Objects to be tested.

- .all:

  Whether to return if all arguments are TRUE.

## Value

`are_named()` and `are_named2()` return a named logical, or unnamed
boolean if `.all` is `TRUE`. `have_names()` is vectorised and returns a
list of logical vectors whhere each is as long as the input object. When
`.all` is `TRUE` for `have_names()`, all logical vectors are collapsed
and a boolean is returned.

## See also

[are-bare-type-predicates](https://lj-jenkins.github.io/favr/reference/are-bare-type-predicates.md)
is_named

## Examples

``` r
# are_named() is a scalar predicate about the whole vector of names:
x <- c(a = 1, b = 2)
are_named(x, c(a = 1, 2))
#>           x c(a = 1, 2) 
#>        TRUE       FALSE 
are_named(x, c(a = 1, 2), .all = TRUE)
#> [1] FALSE

# Unlike are_named2(), are_named() returns `FALSE` for empty vectors
# that don't have a `names` attribute.
are_named(list(), vector())
#>   list() vector() 
#>    FALSE    FALSE 
are_named2(list(), vector())
#>   list() vector() 
#>     TRUE     TRUE 

# have_names() is vectorised
y <- c(a = 1, 2)
have_names(x, y, c(a = 1, 2, 3))
#> $x
#> [1] TRUE TRUE
#> 
#> $y
#> [1]  TRUE FALSE
#> 
#> $`c(a = 1, 2, 3)`
#> [1]  TRUE FALSE FALSE
#> 
have_names(x, y, c(a = 1, 2, 3), .all = TRUE)
#> [1] FALSE

# Empty and missing names are treated as invalid:
invalid <- set_names(letters[1:5])
#> Error in set_names(letters[1:5]): could not find function "set_names"
names(invalid)[1] <- ""
#> Error: object 'invalid' not found
names(invalid)[3] <- NA
#> Error: object 'invalid' not found

are_named(invalid)
#> Error: object 'invalid' not found
have_names(invalid)
#> Error: object 'invalid' not found

# A data frame normally has valid, unique names
# but a matrix usually doesn't because the names
# are stored in a different attribute.
mat <- matrix(1:4, 2)
colnames(mat) <- c("a", "b")
are_named(mtcars, mat)
#> mtcars    mat 
#>   TRUE  FALSE 
have_names(mtcars, mat)
#> $mtcars
#>  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
#> 
#> $mat
#> [1] FALSE FALSE FALSE FALSE
#> 
```
