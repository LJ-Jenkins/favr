# Are objects empty vectors or NULL?

**\[deprecated\]**

These functions were deprecated as they offer little benefit over
[`lapply()`](https://rdrr.io/r/base/lapply.html).

## Usage

``` r
are_empty(..., .all = FALSE)
```

## Arguments

- ...:

  Objects to be tested.

- .all:

  Whether to return if all arguments are TRUE.

## Value

Named logical, or unnamed boolean if `.all` is `TRUE`.

## See also

[is_empty](https://rlang.r-lib.org/reference/is_empty.html)

## Examples

``` r
x <- 1
y <- NULL
z <- list()

are_empty(x, y, z, NULL)
#> Warning: `are_empty()` was deprecated in favr 1.1.0.
#> ℹ Please use `lapply()` with an anonmous function instead.
#>     x     y     z  NULL 
#> FALSE  TRUE  TRUE  TRUE 

are_empty(x, y, z, NULL, .all = TRUE)
#> [1] FALSE

are_empty(list(NULL))
#> list(NULL) 
#>      FALSE 
```
