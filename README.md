
<!-- README.md is generated from README.Rmd. Please edit that file -->

# favr <img id="logo" src="man/figures/logo.png" align="right" height="250" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/favr)](https://CRAN.R-project.org/package=favr)
[![R-CMD-check](https://github.com/LJ-Jenkins/favr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/LJ-Jenkins/favr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Function Argument Validation for R (favr) provides tools for the
succinct validation of function arguments with clear error messaging.

## Overview

- `abortifnot()` and `abortif()` for general validation.
- `check()` for general validation using tidy eval.
- `check_with()` for
  [data-masked](https://rlang.r-lib.org/reference/topic-data-mask.html)
  validation using tidy eval.
- `walk_check()` for applying a check to each element of a vector.

Numerous other `check_*()` functions are provided for specific types of
validation, including:

Validate class and inheritance:

- `check_class()` and `check_inherits()`.

Validate specific types:

- `check_numeric()`, `check_character()`, `check_null()`, etc.
- `check_scalar_numeric()`, `check_scalar_character()`,
  `check_scalar_logical()`, etc.

Validate specific scalar values:

- `check_true()`, `check_false()`, `check_bool()`, `check_string()`.

Modify check behaviour:

- `bare()` to check for bare objects (i.e. objects with no class
  attribute).
- `at_least()`, `at_most()`, and `in_range()` to check for length
  ranges.

Miscellaneous checks:

- `check_dir()` and `check_file()` to check for directory and file
  existence.

## Installation

Install the latest version of favr from CRAN.

``` r
install.packages("favr")
```

### Development Version

To get a bug fix or to use a feature from the development version, you
can install the development version of favr from GitHub.

``` r
# install.packages("pak")
pak::pak("LJ-Jenkins/favr")
```

## Usage

General validation:

``` r
library(favr, warn.conflicts = FALSE)

x <- c(1, 2, 3)
y <- c("a", "b", "c")

abortifnot(x < 4, nchar(y) > 1)
#> Error:
#> ! `nchar(y) > 1` is not TRUE.

abortifnot(
  "{.var x} must be length {.val 5}, but is length {.val {length(x)}}." = length(x) == 5,
  is.character(y)
)
#> Error:
#> ! `x` must be length "5", but is length 3.

abortifnot(
  is.numeric(x),
  is.numeric(y),
  message = "{.var x} and {.var y} must be {.cls character}."
)
#> Error:
#> ! `x` and `y` must be <character>.
```

General validation with tidy evaluation:

``` r
inject_msg <- "{.var x} must contain negative values."

check(is.character(y), {{ inject_msg }} := x < 0)
#> Error:
#> ! `x` must contain negative values.
check(is.character(y), !!inject_msg := x < 0)
#> Error:
#> ! `x` must contain negative values.

inject_args <- list("{.var y} must all have 2 nchars." = nchar(y) == 2)

check(is.numeric(x), !!!inject_args)
#> Error:
#> ! `y` must all have 2 nchars.
```

Data-masked validation:

``` r
df <- data.frame(a = 1:3, b = c("a", "b", "c"))

df |>
  check_with(
    "{.var a} must be length {.val 5}, but is length {.val {length(a)}}." = length(a) == 5,
    "{.var b} must all have 2 nchars." = nchar(b) == 2
  )
#> Error:
#> ! `a` must be length "5", but is length 3.

a <- c("a", "b", "c")

df |>
  check_with(is.numeric(.data$a), is.numeric(.env$a))
#> Error:
#> ! `is.numeric(.env$a)` is not TRUE.
```

Walking a check over a vector:

``` r
x <- list(1, 2, my_el = "3", 4)
walk_check(x, is.numeric)
#> Error:
#> ! Check result for `.x[['my_el']]` (index: 3) is not TRUE.
```

Specific type validation:

``` r
check_integer(x)
#> Error:
#> ! `x` must be an <integer> vector, not a <list>.
check_scalar_double(y)
#> Error:
#> ! `y` must be a scalar <double>, not a <character> vector.

# the `bare()` modifier can be used to ensure bare objects.
check_integer(factor(1))
check_integer(bare(factor(1)))
#> Error:
#> ! `factor(1)` must be a bare <integer>, but it is of class <factor>.

# length modifiers can be used on `n` to specify length ranges.
check_double(x, n = 2)
#> Error:
#> ! `x` must be a <double> vector, not a <list>.
check_double(x, n = at_least(4))
#> Error:
#> ! `x` must be a <double> vector, not a <list>.
check_double(x, n = at_most(2))
#> Error:
#> ! `x` must be a <double> vector, not a <list>.
check_double(x, n = in_range(1, 2))
#> Error:
#> ! `x` must be a <double> vector, not a <list>.
```

Miscellaneous validation:

``` r
check_dir("non_existing_dir")
#> Error:
#> ! `x` must be an existing directory, but it doesn't exist.
#> ℹ Path provided: 'non_existing_dir'.
check_file("non_existing_file")
#> Error:
#> ! `x` must be an existing file, but it doesn't exist.
#> ℹ Path provided: 'non_existing_file'.
```

### Notes

favr relies heavily on the imported packages
[rlang](https://rlang.r-lib.org) and [cli](https://cli.r-lib.org/). For
data validation using user-defined schemas, see
[fluffy](https://lj-jenkins.github.io/fluffy/).

## Getting help

If you encounter a clear bug, please file an issue with a minimal
reproducible example on
[GitHub](https://github.com/LJ-Jenkins/favr/issues).

## Code of Conduct

Please note that the favr project is released with a [Contributor Code
of Conduct](https://lj-jenkins.github.io/favr/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
