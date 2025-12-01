# Are objects empty vectors or NULL?

Are objects empty vectors or NULL?

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

is_empty

## Examples

``` r
x <- 1
y <- NULL
z <- list()

are_empty(x, y, z, NULL)
#>     x     y     z  NULL 
#> FALSE  TRUE  TRUE  TRUE 

are_empty(x, y, z, NULL, .all = TRUE)
#> [1] FALSE

are_empty(list(NULL))
#> list(NULL) 
#>      FALSE 
```
