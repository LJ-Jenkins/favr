# S3 type checks

Check if inputs are expected S3 types and throw an error if not.

## Usage

``` r
check_date(
  x,
  n = NULL,
  ...,
  allow_na = TRUE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_posixct(
  x,
  n = NULL,
  ...,
  allow_na = TRUE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_posixlt(
  x,
  n = NULL,
  ...,
  allow_na = TRUE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_factor(
  x,
  n = NULL,
  ...,
  finite = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_ordered(
  x,
  n = NULL,
  ...,
  finite = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_vctr(
  x,
  n = NULL,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_list_of(
  x,
  n = NULL,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_data_frame(
  x,
  nrow = NULL,
  ncol = NULL,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_tibble(
  x,
  nrow = NULL,
  ncol = NULL,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_data_table(
  x,
  nrow = NULL,
  ncol = NULL,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_tidytable(
  x,
  nrow = NULL,
  ncol = NULL,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)
```

## Arguments

- x:

  An object to check.

- n, nrow, ncol:

  The expected length, number of columns, or number of rows of `x`.

- ...:

  Additional arguments passed to
  [cli_abort()](https://cli.r-lib.org/reference/cli_abort.html) which
  forwards unmatched arguments to
  [abort()](https://rlang.r-lib.org/reference/abort.html).

- allow_na:

  Whether `x` is allowed to contain `NA` values.

- allow_null:

  Whether `x` is allowed to be `NULL`.

- arg:

  An argument name as a string. This argument will be mentioned in error
  messages as the input that is at the origin of a problem.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- finite:

  Whether `x` is required to contain only finite values (i.e. no `NA`,
  `Inf`, `-Inf`, or `NaN`).

## Value

`NULL` invisibly if the check passes, otherwise an error is thrown.

## Details

These functions can be used with the
[`bare()`](https://lj-jenkins.github.io/favr/reference/modifiers.md)
modifier to check if an object is a bare S3 object (where the expected
S3 type is the first class in the class attribute of `x`), and the
length modifiers
[`at_least()`](https://lj-jenkins.github.io/favr/reference/modifiers.md),
[`at_most()`](https://lj-jenkins.github.io/favr/reference/modifiers.md),
and
[`in_range()`](https://lj-jenkins.github.io/favr/reference/modifiers.md)
to modify the behaviour of the length checking `n`, `nrow`, and `ncol`
arguments.

## See also

Other checks:
[`array-type-checks`](https://lj-jenkins.github.io/favr/reference/array-type-checks.md),
[`forbidden-value-checks`](https://lj-jenkins.github.io/favr/reference/forbidden-value-checks.md),
[`inheritance-checks`](https://lj-jenkins.github.io/favr/reference/inheritance-checks.md),
[`path-checks`](https://lj-jenkins.github.io/favr/reference/path-checks.md),
[`scalar-type-checks`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md),
[`scalar-value-checks`](https://lj-jenkins.github.io/favr/reference/scalar-value-checks.md),
[`type-checks`](https://lj-jenkins.github.io/favr/reference/type-checks.md),
[`walk-check`](https://lj-jenkins.github.io/favr/reference/walk-check.md)

## Examples

``` r
x <- as.Date("2000-01-01")
check_date(x)
check_date(1L) |> try()
#> Error in eval(expr, envir) : 
#>   `1L` must inherit from <Date>, but is class <integer>.

class(x) <- c("my_date", class(x))
check_date(bare(x)) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must be a bare <Date>, but it is of class <my_date>.

x <- x + 1:5
check_date(x, n = 3) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must be a <Date> vector of length 3, not 5.
check_date(x, n = at_least(10)) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must be a <Date> vector of at least length 10, but it is of length
#> 5.
check_date(x, n = at_most(3)) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must be a <Date> vector of at most length 3, but it is of length 5.
check_date(x, n = in_range(6, 10)) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must be a <Date> vector of a length between 6 and 10, but it is of
#> length 5.

x <- data.frame(x = 1:3, y = 1:3)
check_tibble(x) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must inherit from <tbl_df>, but is class <data.frame>.

class(x) <- c("my_tbl", "tbl_df", class(x))
check_tibble(x)
check_tibble(bare(x)) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must be a bare <tbl_df>, but it is of class <my_tbl>.

check_tibble(x, nrow = 2) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must be a <tbl_df> with 2 rows, not 3.
check_tibble(x, nrow = at_least(4)) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must be a <tbl_df> with at least 4 rows, but it has 3.
check_tibble(x, nrow = in_range(1, 2)) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must be a <tbl_df> with 1 to 2 rows, but it has 3.
check_tibble(x, ncol = 3) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must be a <tbl_df> with 3 rows, not 2.
check_tibble(x, ncol = at_most(1)) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must be a <tbl_df> with at most 1 column, but it has 2.
check_tibble(x, ncol = in_range(3, 5)) |> try()
#> Error in eval(expr, envir) : 
#>   `x` must be a <tbl_df> with 3 to 5 columns, but it has 2.
```
