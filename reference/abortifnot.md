# Ensure the truth of R expressions

If any of the expressions in `...` are not
[all](https://rdrr.io/r/base/all.html) `TRUE`,
[cli_abort()](https://cli.r-lib.org/reference/cli_abort.html) is called,
producing an error message indicating the first expression which was not
([all](https://rdrr.io/r/base/all.html)) `TRUE`.

For `abortif()`, the opposite is true, i.e. expressions should evaluate
to ([all](https://rdrr.io/r/base/all.html)) `FALSE` for no error to
occur.

## Usage

``` r
abortifnot(
  ...,
  message = NULL,
  call = .envir,
  .envir = parent.frame(),
  .frame = .envir,
  abort_args = NULL
)

abortif(
  ...,
  message = NULL,
  call = .envir,
  .envir = parent.frame(),
  .frame = .envir,
  abort_args = NULL
)
```

## Arguments

- ...:

  Any number of R expressions, which should each evaluate to (a
  [`logical`](https://rdrr.io/r/base/logical.html) vector of **all**)
  [`TRUE`](https://rdrr.io/r/base/logical.html) for no error to occur
  (`FALSE` for `abortif()`). Non-`logical` and `NA` values will trigger
  an error.

  If an expression is named, the name will be used in the error message
  instead of the default message or the `message` argument.

- message:

  Default error message for non-named expressions.

- call:

  An execution environment, defused function call, or `NULL`. Passed to
  [cli_abort()](https://cli.r-lib.org/reference/cli_abort.html).

- .envir:

  Environment to evaluate the cli formatting of the error message in.
  Passed to
  [cli_abort()](https://cli.r-lib.org/reference/cli_abort.html).

- .frame:

  The throwing context. Passed to
  [cli_abort()](https://cli.r-lib.org/reference/cli_abort.html).

- abort_args:

  A list of additional arguments to pass to
  [abort()](https://rlang.r-lib.org/reference/abort.html) (forwarded
  from [cli_abort()](https://cli.r-lib.org/reference/cli_abort.html)).

## Value

`NULL`, called for side effects only.

## See also

[stopifnot](https://rdrr.io/r/base/stopifnot.html) for the base **R**
function this is based on.

[check](https://lj-jenkins.github.io/favr/reference/check.md) and
[check_with](https://lj-jenkins.github.io/favr/reference/check.md) for a
non data-masked and data-masked version of `abortifnot()` with tidy
evaluation and
[injection](https://rlang.r-lib.org/reference/topic-inject.html)
support.

## Examples

``` r
abortifnot(1 == 1, all.equal(pi, 3.14159265), 1 < 2) # all TRUE

m <- matrix(c(1, 3, 3, 1), 2, 2)
abortifnot(m == t(m), diag(m) == rep(1, 2)) # all TRUE

abortifnot(1) |> try()
#> Error in eval(expr, envir) : 
#>   `1` must be a <logical> vector, not the number 1.

# A custom error message can be given for each expression:
m[1, 2] <- 12
abortifnot("{.var m} must be {.cls symmetric}" = m == t(m)) |>
  try()
#> Error in eval(expr, envir) : `m` must be <symmetric>

# Alternatively, one error message can be used for all
# expressions.
abortifnot(
  m[1, 1] == 1,
  diag(m) == rep(2, 2),
  message = "{.var m} has a diagonal of: {diag(m)}"
) |> try()
#> Error in eval(expr, envir) : `m` has a diagonal of: 1 and 1

# The `call` argument can be used to specify where the
# error occurs, by default this is the caller environment.
myfunc <- function(x) abortifnot(x)
myfunc(FALSE) |> try()
#> Error in myfunc(FALSE) : `x` is not TRUE.

# abortif() errors if any argument does not evaluate to
# (all) FALSE.
abortif(c(T, F)) |> try()
#> Error in eval(expr, envir) : `c(T, F)` is TRUE.
abortif(c(T, NA)) |> try()
#> Error in eval(expr, envir) : 
#>   `c(T, NA)` must not contain NA values.
```
