# favr

Function Argument Validation for R (favr) provides tools for the
succinct validation of function arguments with clear error messaging.

## Overview

- [`abortifnot()`](https://lj-jenkins.github.io/favr/reference/abortifnot.md)
  and
  [`abortif()`](https://lj-jenkins.github.io/favr/reference/abortifnot.md)
  for general validation.
- [`check()`](https://lj-jenkins.github.io/favr/reference/check.md) for
  general validation using tidy evaluation.
- [`check_with()`](https://lj-jenkins.github.io/favr/reference/check.md)
  for
  [data-masked](https://rlang.r-lib.org/reference/topic-data-mask.html)
  validation using tidy evaluation.
- [`walk_check()`](https://lj-jenkins.github.io/favr/reference/walk-check.md)
  for applying a check to each element of a vector.
- [`check_class()`](https://lj-jenkins.github.io/favr/reference/inheritance-checks.md)
  and
  [`check_inherits()`](https://lj-jenkins.github.io/favr/reference/inheritance-checks.md)
  for class validation.

Numerous other strongly typed `check_*()` functions are provided for
specific types of validation, including:

Validate specific types:

- [`check_integer()`](https://lj-jenkins.github.io/favr/reference/type-checks.md),
  [`check_character()`](https://lj-jenkins.github.io/favr/reference/type-checks.md),
  [`check_null()`](https://lj-jenkins.github.io/favr/reference/type-checks.md),
  etc.
- [`check_scalar_integer()`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md),
  [`check_scalar_character()`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md),
  [`check_scalar_logical()`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md),
  etc.
- [`check_array()`](https://lj-jenkins.github.io/favr/reference/array-type-checks.md)
  and
  [`check_matrix()`](https://lj-jenkins.github.io/favr/reference/array-type-checks.md)
  for the ‘implicit’ types of array and matrix, respectively.

Validate specific S3 types:

- [`check_factor()`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md),
  [`check_date()`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md),
  [`check_posixct()`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md),
  etc.
- `check_data.frame()`,
  [`check_tibble()`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md),
  `check_data.table()`, etc.
- [`check_vctr()`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md)
  and
  [`check_list_of()`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md)
  for their respective [vctrs](https://vctrs.r-lib.org) classes.

Validate OOP types:

- [`check_s3()`](https://lj-jenkins.github.io/favr/reference/oop-checks.md),
  [`check_s4()`](https://lj-jenkins.github.io/favr/reference/oop-checks.md),
  [`check_s7()`](https://lj-jenkins.github.io/favr/reference/oop-checks.md)
  and
  [`check_r6()`](https://lj-jenkins.github.io/favr/reference/oop-checks.md)
  for their respective OOP types.

Modify the behaviour of type-checking functions:

- [`bare()`](https://lj-jenkins.github.io/favr/reference/modifiers.md)
  to also check for bare objects (i.e. objects with no class attribute)
  in the type check functions, or bare S3 objects (where the expected S3
  class is first in the class attribute vector) in the S3 type check
  functions.
- [`at_least()`](https://lj-jenkins.github.io/favr/reference/modifiers.md),
  [`at_most()`](https://lj-jenkins.github.io/favr/reference/modifiers.md),
  and
  [`in_range()`](https://lj-jenkins.github.io/favr/reference/modifiers.md)
  to also check for ranges in length/number of rows/number of columns.

Validate specific scalar values:

- [`check_true()`](https://lj-jenkins.github.io/favr/reference/scalar-value-checks.md),
  [`check_false()`](https://lj-jenkins.github.io/favr/reference/scalar-value-checks.md),
  [`check_bool()`](https://lj-jenkins.github.io/favr/reference/scalar-value-checks.md)
  and
  [`check_string()`](https://lj-jenkins.github.io/favr/reference/scalar-value-checks.md).

Validate the lack of forbidden values:

- [`check_no_na()`](https://lj-jenkins.github.io/favr/reference/forbidden-value-checks.md),
  [`check_finite()`](https://lj-jenkins.github.io/favr/reference/forbidden-value-checks.md),
  [`check_unique()`](https://lj-jenkins.github.io/favr/reference/forbidden-value-checks.md)
  and
  [`check_nzchar()`](https://lj-jenkins.github.io/favr/reference/forbidden-value-checks.md).

Validate object properties:

- [`check_length()`](https://lj-jenkins.github.io/favr/reference/property-checks.md),
  [`check_nrow()`](https://lj-jenkins.github.io/favr/reference/property-checks.md),
  [`check_ncol()`](https://lj-jenkins.github.io/favr/reference/property-checks.md),
  [`check_size()`](https://lj-jenkins.github.io/favr/reference/property-checks.md),
  [`check_non_empty()`](https://lj-jenkins.github.io/favr/reference/property-checks.md)
  and
  [`check_named()`](https://lj-jenkins.github.io/favr/reference/property-checks.md).

Validate file and directory existence:

- [`check_dir()`](https://lj-jenkins.github.io/favr/reference/path-checks.md),
  [`check_file()`](https://lj-jenkins.github.io/favr/reference/path-checks.md)
  and
  [`check_ext()`](https://lj-jenkins.github.io/favr/reference/path-checks.md).

Build checks in the style of favr:

- [`s3_vec_check()`](https://lj-jenkins.github.io/favr/reference/s3-check-builders.md)
  and
  [`s3_df_check()`](https://lj-jenkins.github.io/favr/reference/s3-check-builders.md)
  for developers to create their own S3 type checks.

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
  message = "{.var x} and {.var y} must be {.cls numeric}."
)
#> Error:
#> ! `x` and `y` must be <numeric>.
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

data <- list(a = c("a", "b", "c"), b = 1:3)

# `check_with()` user-supplied messages are eval'd in the data mask context.
check_with(data,
  "{.var a} must all have 1 nchars." = nchar(a) == 1,
  "{.var b} must be length {.val 5}, but is length {.val {length(b)}}." = length(b) == 5
)
#> Error:
#> ! `b` must be length "5", but is length 3.

b <- c("a", "b", "c")

check_with(data, is.numeric(.data$b), is.numeric(.env$b))
#> Error:
#> ! `is.numeric(.env$b)` is not TRUE.
```

Walking a check over a vector:

``` r

x <- list(1, 2, my_el = "3", 4)
walk_check(x, is.numeric)
#> Error:
#> ! Check result for `.x[['my_el']]` (index: 3) is not TRUE.
```

Class validation:

``` r

x <- structure(1:3, class = "a_class")
check_class(x, "my_class")
#> Error:
#> ! `x` must be class <my_class>, but is class <a_class>.
class(x) <- c("b_class", class(x))
check_inherits(x, "my_class")
#> Error:
#> ! `x` must inherit from <my_class>, but is class <b_class/a_class>.
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
check_s3(x)
#> Error:
#> ! `x` must be an <S3> object, not <numeric>.

df <- data.frame(x = 1:3, y = 1:3)
check_s3(df)
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

Ensure no forbidden values:

``` r

x <- c(1, 2, 1, NA)
check_no_na(x)
#> Error:
#> ! `x` must not contain NA values.
check_finite(x)
#> Error:
#> ! `x` must not contain non-finite values.
check_unique(x)
#> Error:
#> ! `x` must have unique elements. Duplicates: 1.

x <- c("a", "b", "")
check_nzchar(x)
#> Error:
#> ! `x` must not contain empty strings.
x <- c("a", "b", " ")
check_nzchar(x, allow_all_ws = FALSE)
#> Error:
#> ! `x` must not contain all whitespace elements.
```

Check object properties:

``` r

x <- c(1, 2, 3)
check_length(x, 2)
#> Error:
#> ! `x` must be of length 2, not 3.
check_size(x, at_most(1))
#> Error:
#> ! `x` must be of at most size 1, but it is of size 3.
df <- data.frame(x = 1:3, y = 1:3)
check_nrow(df, 2)
#> Error:
#> ! `df` must have 2 rows, not 3.
check_ncol(df, in_range(3, 5))
#> Error:
#> ! `df` must have 3 to 5 columns, but it has 2.
x <- numeric(0)
check_non_empty(x)
#> Error:
#> ! `x` must not be empty.
x <- c(1, 2, 3)
check_named(x)
#> Error:
#> ! `x` must be named.
names(x) <- c("a", "b", "a")
check_named(x, unique = TRUE)
#> Error:
#> ! `x` must have unique names. Duplicates: "a".
names(x) <- c("a", "b", "")
check_named(x, allow_empty = FALSE)
#> Error:
#> ! `x` must not contain empty names.
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
  s3_vec_check(
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
[rlang](https://rlang.r-lib.org) and [cli](https://cli.r-lib.org/).

For data validation using user-defined schemas, see
[fluffy](https://lj-jenkins.github.io/fluffy/).

## Getting help

If you encounter a clear bug, please file an issue with a minimal
reproducible example on
[GitHub](https://github.com/LJ-Jenkins/favr/issues).

## Code of Conduct

Please note that the favr project is released with a [Contributor Code
of Conduct](https://lj-jenkins.github.io/favr/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
