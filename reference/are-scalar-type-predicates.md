# Scalar type predicates

Wrappers around rlang scalar type predicates that allow multiple objects
to be passed. The following documentation is adapted from the rlang
[documentation](https://rlang.r-lib.org/reference/scalar-type-predicates.html):

These predicates check for a given type and whether the vector is
"scalar", that is, of length 1.

In addition to the length check, `are_string()` and `are_bool()` return
`FALSE` if their input is missing. This is useful for type-checking
arguments, when your function expects a single string or a single `TRUE`
or `FALSE`.

## Usage

``` r
are_scalar_list(..., .all = FALSE)

are_scalar_atomic(..., .all = FALSE)

are_scalar_vector(..., .all = FALSE)

are_scalar_integer(..., .all = FALSE)

are_scalar_double(..., .all = FALSE)

are_scalar_complex(..., .all = FALSE)

are_scalar_character(..., .all = FALSE)

are_string(..., .string = NULL, .all = FALSE)

are_scalar_logical(..., .all = FALSE)

are_bool(..., .all = FALSE)

are_scalar_raw(..., .all = FALSE)

are_scalar_bytes(..., .all = FALSE)
```

## Arguments

- ...:

  Objects to be tested.

- .all:

  Whether to return if all arguments are TRUE.

- .string:

  A string/character vector to compare to the inputs.

## Value

Named logical, or unnamed boolean if `.all` is `TRUE`.

## Details

The optional input of `.string` can be given character vectors that map
to the arguments in `...`. If unnamed vector/list, the input must either
be the same length as the number of arguments given to `...`, or length
1: which is then recycled to the number number of arguments given to
`...`. Alternatively, a named vector/list can be given, where the values
for matching named elements are passed to the type predicate, but
unmatched names are passed NULL. List inputs can pass different
character vectors for each dot argument. When a character vector is
given for a single argument, `TRUE` is returned if at least one element
is equal.

## See also

[are-type-predicates](https://lj-jenkins.github.io/favr/reference/are-type-predicates.md),
[are-bare-type-predicates](https://lj-jenkins.github.io/favr/reference/are-bare-type-predicates.md)

## Examples

``` r
x <- 1
y <- list()
z <- mean

are_scalar_list(x, y, z, list(1))
#>       x       y       z list(1) 
#>   FALSE   FALSE   FALSE    TRUE 

# `.all` can be given to test if all inputs
# evaluate to TRUE
are_list(x, y, z, list(1), .all = TRUE)
#> [1] FALSE
```
