
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
- `check()` for general validation using tidy evaluation.
- `check_with()` for
  [data-masked](https://rlang.r-lib.org/reference/topic-data-mask.html)
  validation using tidy evaluation.
- `walk_check()` for applying a check to each element of a vector.

Numerous other `check_*()` functions are provided for specific types of
validation, including:

Validate class and inheritance:

- `check_class()` and `check_inherits()`.

Validate specific types:

- `check_integer()`, `check_character()`, `check_null()`, etc.
- `check_scalar_integer()`, `check_scalar_character()`,
  `check_scalar_logical()`, etc.
- `check_array()` and `check_matrix()` for the ‘implicit’ types of array
  and matrix, respectively.

Validate specific S3 types:

- `check_factor()`, `check_date()`, `check_posixct()`, etc.
- `check_data.frame()`, `check_tibble()`, `check_data.table()`, etc.
- `check_vctr()` and `check_list_of()` for their respective
  [vctrs](https://vctrs.r-lib.org) classes.
- `check_s3_vec()` and `check_s3_df()` for developers to create their
  own S3 type checks.

Modify the behaviour of type-checking functions:

- `bare()` to also check for bare objects (i.e. objects with no class
  attribute) in the type check functions, or bare S3 objects (where the
  expected S3 class is first in the class attribute vector) in the S3
  type check functions.
- `at_least()`, `at_most()`, and `in_range()` to also check for ranges
  in length/number of rows/number of columns.

Validate specific scalar values:

- `check_true()`, `check_false()`, `check_bool()`, `check_string()`.

Validate file and directory existence:

- `check_dir()`, `check_file()` and `check_ext()`.

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
  "{.var x} must be length {.val {5}}, but is length {.val {length(x)}}." = length(x) == 5,
  is.character(y)
)
#> Error:
#> ! `x` must be length 5, but is length 3.

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
data <- data.frame(a = 1:3, b = c("a", "b", "c"))

# `check_with()` user-supplied messages are eval'd in the data mask context.
check_with(data,
  "{.var a} must be length {.val 5}, but is length {.val {length(a)}}." = length(a) == 5,
  "{.var b} must all have 2 nchars." = nchar(b) == 2
)
#> Error:
#> ! `a` must be length "5", but is length 3.

a <- c("a", "b", "c")

check_with(data, is.numeric(.data$a), is.numeric(.env$a))
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
x <- c(1, 2, 3)
check_integer(x)
#> Error:
#> ! `x` must be an <integer> vector, not a <double> vector.
check_scalar_double(x)
#> Error:
#> ! `x` must be a scalar <double>, but it is of length 3.

df <- data.frame(x = 1:3, y = 1:3)
check_tibble(df)
#> Error:
#> ! `df` must inherit from <tbl_df>, but is class <data.frame>.

# the `bare()` modifier can be used to ensure bare objects.
check_integer(factor(1))
check_integer(bare(factor(1)))
#> Error:
#> ! `factor(1)` must be a bare <integer>, but it is of class <factor>.

class(df) <- c("my_class", "tbl_df", "tbl", class(df))
check_tibble(df)
check_tibble(bare(df))
#> Error:
#> ! `df` must be a bare <tbl_df>, but it is of class <my_class>.

# length modifiers can be used on `n` to specify length ranges.
check_double(x, n = 2)
#> Error:
#> ! `x` must be a <double> vector of length 2, not 3.
check_double(x, n = at_least(4))
#> Error:
#> ! `x` must be a <double> vector of at least length 4, but it is of
#>   length 3.
check_double(x, n = at_most(2))
#> Error:
#> ! `x` must be a <double> vector of at most length 2, but it is of length
#>   3.
check_double(x, n = in_range(1, 2))
#> Error:
#> ! `x` must be a <double> vector of a length between 1 and 2, but it is
#>   of length 3.

check_tibble(df, nrow = 2)
#> Error:
#> ! `df` must be a <tbl_df> with 2 rows, not 3.
check_tibble(df, ncol = at_least(3))
#> Error:
#> ! `df` must be a <tbl_df> with at least 3 columns, but it has 2.
check_tibble(df, nrow = at_most(2))
#> Error:
#> ! `df` must be a <tbl_df> with at most 2 rows, but it has 3.
check_tibble(df, ncol = in_range(3, 5))
#> Error:
#> ! `df` must be a <tbl_df> with 3 to 5 columns, but it has 2.
```

File/dir existence validation:

``` r
check_dir("non_existing_dir")
#> Error:
#> ! `x` must be an existing directory, but it doesn't exist.
#> ℹ Path provided: 'non_existing_dir'.
check_file("non_existing_file")
#> Error:
#> ! `x` must be an existing file, but it doesn't exist.
#> ℹ Path provided: 'non_existing_file'.
check_ext("file.txt", ext = c(".csv", ".xlsx"))
#> Error:
#> ! `"file.txt"` must have extension ".csv" or ".xlsx".
check_file("file.txt", ext = c(".csv", ".xlsx"))
#> Error:
#> ! `"file.txt"` must have extension ".csv" or ".xlsx".
```

Build your own S3 type checks:

``` r
check_my_class <- function(
  x,
  n = NULL,
  ...,
  allow_null = FALSE,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  check_s3_vec(
    x,
    n,
    type = "my_class",
    type_msg = "a {.cls my_class} vector",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_my_class(1L)
#> Error:
#> ! `1L` must inherit from <my_class>, but is class <integer>.

x <- structure(1:3, class = "my_class")
check_my_class(x)

check_my_class(NULL, allow_null = TRUE)

class(x) <- c("another_class", class(x))
check_my_class(bare(x))
#> Error:
#> ! `x` must be a bare <my_class>, but it is of class <another_class>.

check_my_class(x, n = at_most(2))
#> Error:
#> ! `x` must be a <my_class> vector of at most length 2, but it is of
#>   length 3.
check_my_class(x, n = in_range(1, 2))
#> Error:
#> ! `x` must be a <my_class> vector of a length between 1 and 2, but it is
#>   of length 3.
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
