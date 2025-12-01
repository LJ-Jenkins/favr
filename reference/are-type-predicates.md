# Type predicates

Wrappers around
[rlang](https://rlang.r-lib.org/reference/rlang-package.html) type
predicates that allow multiple objects to be passed. The following
documentation is adapted from the rlang
[documentation](https://rlang.r-lib.org/reference/type-predicates.html):

These type predicates aim to make type testing in R more consistent.
They are wrappers around
[`base::typeof()`](https://rdrr.io/r/base/typeof.html), so operate at a
level beneath S3/S4 etc.

Compared to base R functions:

- The predicates for vectors include the `.n` argument for
  pattern-matching on the vector length.

- Unlike [`is.atomic()`](https://rdrr.io/r/base/is.recursive.html) in R
  \< 4.4.0, `are_atomic()` does not return `TRUE` for `NULL`. Starting
  in R 4.4.0 `is.atomic(NULL)` returns FALSE.

- Unlike [`is.vector()`](https://rdrr.io/r/base/vector.html),
  `are_vector()` tests if an object is an atomic vector or a list.
  `is.vector` checks for the presence of attributes (other than name).

## Usage

``` r
are_list(..., .n = NULL, .all = FALSE)

are_atomic(..., .n = NULL, .all = FALSE)

are_vector(..., .n = NULL, .all = FALSE)

are_integer(..., .n = NULL, .all = FALSE)

are_double(..., .n = NULL, .finite = NULL, .all = FALSE)

are_complex(..., .n = NULL, .finite = NULL, .all = FALSE)

are_character(..., .n = NULL, .all = FALSE)

are_logical(..., .n = NULL, .all = FALSE)

are_raw(..., .n = NULL, .all = FALSE)

are_bytes(..., .n = NULL, .all = FALSE)

are_null(..., .all = FALSE)
```

## Arguments

- ...:

  Objects to be tested.

- .n:

  Expected lengths of the vectors.

- .all:

  If `TRUE`, return boolean of whether all arguments returned `TRUE`.

- .finite:

  Whether all values of the vectors are finite. The non-finite values
  are `NA`, `Inf`, `-Inf` and `NaN`. Setting this to something other
  than `NULL` can be expensive because the whole vector needs to be
  traversed and checked.

## Value

Named logical, or unnamed boolean if `.all` is `TRUE`.

## Details

The optional inputs of `.n` and `.finite` can be given inputs that map
to the arguments in `...`. If a unnamed vector/list, the input must
either be the same length as the number of arguments given to `...`, or
length 1: which is then recycled to the number number of arguments given
to `...`. Alternatively, a named vector/list can be given, where the
values for matching named elements are passed to the type predicate, but
unmatched names are passed NULL.

## See also

[are-bare-type-predicates](https://lj-jenkins.github.io/favr/reference/are-bare-type-predicates.md)
[are-scalar-type-predicates](https://lj-jenkins.github.io/favr/reference/are-scalar-type-predicates.md)

## Examples

``` r
x <- 1
y <- list()
z <- mean

are_list(x, y, z, list(1))
#>       x       y       z list(1) 
#>   FALSE    TRUE   FALSE    TRUE 

# `.all` can be given to test if all inputs
# evaluate to TRUE
are_list(x, y, z, list(1), .all = TRUE)
#> [1] FALSE

# scalar inputs to `.n` and `.finite` are
# recycled to number of inputs
are_list(x, y, z, list(1), .n = 1)
#>       x       y       z list(1) 
#>   FALSE   FALSE   FALSE    TRUE 

# inputs to `.n` and `.finite` matching the
# number of inputs are applied sequentially
are_list(x, y, z, list(1), .n = c(1, 0, 1, 2))
#>       x       y       z list(1) 
#>   FALSE    TRUE   FALSE   FALSE 

# named inputs to `.n` and `.finite` are applied
# to the matching input names, with the other inputs
# being given NULL
are_list(x, y, z, list(1), .n = c(y = 1, "list(1)" = 2))
#>       x       y       z list(1) 
#>   FALSE   FALSE   FALSE   FALSE 
```
