# Bare type predicates

Wrappers around rlang type predicates that allow multiple objects to be
passed. The following documentation is adapted from the rlang
[documentation](https://rlang.r-lib.org/reference/bare-type-predicates.html):

These predicates check for a given type but only return `TRUE` for bare
R objects. Bare objects have no class attributes. For example, a data
frame is a list, but not a bare list.

- The predicates for vectors include the `.n` argument for
  pattern-matching on the vector length.

- Like
  [`are_atomic()`](https://lj-jenkins.github.io/favr/reference/are-type-predicates.md)
  and unlike base R
  [`is.atomic()`](https://rdrr.io/r/base/is.recursive.html) for R \<
  4.4.0, `are_bare_atomic()` does not return `TRUE` for `NULL`. Starting
  in R 4.4.0, `is.atomic(NULL)` returns FALSE.

- Unlike base R [`is.numeric()`](https://rdrr.io/r/base/numeric.html),
  `are_bare_double()` only returns `TRUE` for floating point numbers.

## Usage

``` r
are_bare_list(..., .n = NULL, .all = FALSE)

are_bare_atomic(..., .n = NULL, .all = FALSE)

are_bare_vector(..., .n = NULL, .all = FALSE)

are_bare_integer(..., .n = NULL, .all = FALSE)

are_bare_double(..., .n = NULL, .all = FALSE)

are_bare_complex(..., .n = NULL, .all = FALSE)

are_bare_character(..., .n = NULL, .all = FALSE)

are_bare_string(..., .n = NULL, .all = FALSE)

are_bare_logical(..., .n = NULL, .all = FALSE)

are_bare_raw(..., .n = NULL, .all = FALSE)

are_bare_bytes(..., .n = NULL, .all = FALSE)

are_bare_numeric(..., .n = NULL, .all = FALSE)
```

## Arguments

- ...:

  Objects to be tested.

- .n:

  Expected lengths of the vectors.

- .all:

  Whether to return if all arguments are TRUE.

## Value

Named logical, or unnamed boolean if `.all` is `TRUE`.

## Details

The optional input of `.n` can be given values that map to the arguments
in `...`. If a unnamed vector/list, the input must either be the same
length as the number of arguments given to `...`, or length 1: which is
then recycled to the number number of arguments given to `...`.
Alternatively, a named vector/list can be given, where the values for
matching named elements are passed to the type predicate, but unmatched
names are passed NULL.

## See also

[are-type-predicates](https://lj-jenkins.github.io/favr/reference/are-type-predicates.md),
[are-scalar-type-predicates](https://lj-jenkins.github.io/favr/reference/are-scalar-type-predicates.md)

## Examples

``` r
x <- 1
y <- list()
class(y) <- c("my_class", class(y))
z <- mean

are_bare_list(x, y, z, list(1))
#>       x       y       z list(1) 
#>   FALSE   FALSE   FALSE    TRUE 

# `.all` can be given to test if all inputs
# evaluate to TRUE
are_bare_list(x, y, z, list(1), .all = TRUE)
#> [1] FALSE

# scalar inputs to `.n` are recycled to number of inputs
are_bare_list(x, y, z, list(1), .n = 2)
#>       x       y       z list(1) 
#>   FALSE   FALSE   FALSE   FALSE 

# inputs to `.n` matching the number of inputs
# are applied sequentially
are_bare_list(list(), y, list(1, 2, 3), list(1), .n = c(0, 0, 3, 1))
#>        list()             y list(1, 2, 3)       list(1) 
#>          TRUE         FALSE          TRUE          TRUE 

# named inputs to `.n` are applied to the matching input
# names, with the other inputs being given NULL
x <- list()
are_bare_list(x, y, list(1, 2, 3), list(1), .n = c(x = 5, "list(1)" = 2))
#>             x             y list(1, 2, 3)       list(1) 
#>         FALSE         FALSE          TRUE         FALSE 
```
