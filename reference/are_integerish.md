# Are vectors integer-like?

Wrappers around rlang type predicates that allow multiple objects to be
passed. The following documentation is adapted from the rlang
documentation:

These predicates check whether R considers a number vector to be
integer-like, according to its own tolerance check (which is in fact
delegated to the C library). This function is not adapted to data
analysis, see the help for
[`base::is.integer()`](https://rdrr.io/r/base/integer.html) for examples
of how to check for whole numbers.

Things to consider when checking for integer-like doubles:

- This check can be expensive because the whole double vector has to be
  traversed and checked.

- Large double values may be integerish but may still not be coercible
  to integer. This is because integers in R only support values up to
  `2^31 - 1` while numbers stored as double can be much larger.

## Usage

``` r
are_integerish(..., .n = NULL, .finite = NULL, .all = FALSE)

are_scalar_integerish(..., .n = NULL, .finite = NULL, .all = FALSE)

are_bare_integerish(..., .n = NULL, .finite = NULL, .all = FALSE)
```

## Arguments

- ...:

  Objects to be tested.

- .n:

  Expected lengths of the vectors.

- .finite:

  Whether all values of the vectors are finite. The non-finite values
  are `NA`, `Inf`, `-Inf` and `NaN`. Setting this to something other
  than `NULL` can be expensive because the whole vector needs to be
  traversed and checked.

- .all:

  If `TRUE`, return boolean of whether all arguments returned `TRUE`.

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

is_bare_numeric and
[are_bare_numeric](https://lj-jenkins.github.io/favr/reference/are-bare-type-predicates.md)
for testing whether an object is a base numeric type (a bare double or
integer vector).

## Examples

``` r
x <- 10L
y <- 10.0
z <- 10.000001

are_integerish(x, y, z, TRUE)
#>     x     y     z  TRUE 
#>  TRUE  TRUE FALSE FALSE 

#' # `.all` can be given to test if all inputs
# evaluate to TRUE
are_integerish(x, y, z, TRUE, .all = TRUE)
#> [1] FALSE

# scalar inputs to `.n` and `.finite` are
# recycled to number of inputs
are_integerish(x, y, z, TRUE, .n = 2)
#>     x     y     z  TRUE 
#> FALSE FALSE FALSE FALSE 

# inputs to `.n` and `.finite` matching the
# number of inputs are applied sequentially
are_integerish(x, y, z, TRUE, .n = c(1, 2, 1, 1))
#>     x     y     z  TRUE 
#>  TRUE FALSE FALSE FALSE 

# named inputs to `.n` and `.finite` are applied
# to the matching input names, with the other inputs
# being given NULL
are_integerish(x, y, z, TRUE, .n = c(y = 2, "TRUE" = 1))
#>     x     y     z  TRUE 
#>  TRUE FALSE FALSE FALSE 
```
