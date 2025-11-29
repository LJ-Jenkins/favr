# Are objects TRUE or FALSE?

Test if any number of inputs are TRUE or FALSE. Inputs are passed to
[isTRUE](https://rdrr.io/r/base/Logic.html) or
[isFALSE](https://rdrr.io/r/base/Logic.html).

## Usage

``` r
are_true(..., .all = FALSE)

are_false(..., .all = FALSE)
```

## Arguments

- ...:

  Objects to be tested.

- .all:

  Whether to return if all arguments are TRUE.

## Value

Named logical, or unnamed boolean if `.all` is `TRUE`.

## See also

[isTRUE](https://rdrr.io/r/base/Logic.html)
[isFALSE](https://rdrr.io/r/base/Logic.html)

## Examples

``` r
x <- TRUE
y <- 1
z <- mean

are_true(x, y, z, TRUE, 0)
#>     x     y     z  TRUE     0 
#>  TRUE FALSE FALSE  TRUE FALSE 

are_true(x, y, z, TRUE, 0, .all = TRUE)
#> [1] FALSE

are_false(x, y, z, TRUE, 0)
#>     x     y     z  TRUE     0 
#> FALSE FALSE FALSE FALSE FALSE 

are_false(x, y, z, TRUE, 0, .all = TRUE)
#> [1] FALSE
```
