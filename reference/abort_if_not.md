# Ensure the truth of R expressions

If any of the expressions in `...` are not all `TRUE`,
[rlang::abort](https://rlang.r-lib.org/reference/abort.html) is called
for the first expression which was not
([all](https://rdrr.io/r/base/all.html)) `TRUE`. The names of
expressions can be used as the error message or a single default error
message can be given using `.message`. Both are passed to
[cli::format_inline](https://cli.r-lib.org/reference/format_inline.html)
for formatting.

## Usage

``` r
abort_if_not(..., .message = NULL, .error_call = caller_env())

abort_if(..., .message = NULL, .error_call = caller_env())
```

## Arguments

- ...:

  any number of R expressions, which should each evaluate to (a logical
  vector of [all](https://rdrr.io/r/base/all.html)) `TRUE` for no error
  to occur. Positive numbers are not `TRUE`, even when they are coerced
  to `TRUE` inside `if()` or in arithmetic computations in R. If the
  expressions are named, the names will be used in the error message.

- .message:

  single default error message for non-named expressions.

- .error_call:

  the call environment to use for error messages (passed to
  [rlang::abort](https://rlang.r-lib.org/reference/abort.html)).

## Value

NULL, called for side effects only.

## Details

abort_if is the opposite of abort_if_not, i.e. expressions should
evaluate to ([all](https://rdrr.io/r/base/all.html)) `FALSE` for no
error to occur. See
[enforce](https://lj-jenkins.github.io/favr/reference/enforce.md) and
[schema](https://lj-jenkins.github.io/favr/reference/schema.md) for a
non data-masked and data-masked version of abort_if_not with options for
size recycling and type casting.

## Examples

``` r
# NB: Some of these examples are expected to produce an error. To
#     prevent them from terminating a run with example() they are
#     piped into a call to try().

abort_if_not(1 == 1, all.equal(pi, 3.14159265), 1 < 2) # all TRUE

m <- matrix(c(1, 3, 3, 1), 2, 2)
abort_if_not(m == t(m), diag(m) == rep(1, 2)) # all TRUE

abort_if_not(1) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `abort_if_not()`.
#> ℹ In argument: `1`.
#> ! Argument must be <logical>, not <numeric>.
# Error:
# Caused by error in `abort_if_not()`:
# ℹ In argument: `1`.
# ! Argument must be <logical>, not <numeric>.

# A custom error message can be given for each expression:
m[1, 2] <- 12
abort_if_not("{.var m} must be {.cls symmetric}" = m == t(m)) |>
  try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `abort_if_not()`.
#> ℹ In argument: `m == t(m)`.
#> ! `m` must be <symmetric>
# Error:
# Caused by error in `abort_if_not()`:
# ℹ In argument: `m == t(m)`.
# ! `m` must be <symmetric>

# Alternatively, one error message can be used for all
# expressions:
abort_if_not(
  m[1, 1] == 1,
  diag(m) == rep(2, 2),
  .message = "{.var m} has a diagonal of: {diag(m)}"
) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `abort_if_not()`.
#> ℹ In argument: `diag(m) == rep(2, 2)`.
#> ! `m` has a diagonal of: 1 and 1
# Error:
# Caused by error in `abort_if_not()`:
# ℹ In argument: `diag(m) == rep(2, 2)`.
# ! `m` must have a diagonal of: 1 and 1

# The `.error_call` argument can be used to specify where the
# error occurs, by default this is the caller environment:
myfunc <- function(x) abort_if_not(x)
myfunc(FALSE) |> try()
#> Error in myfunc(FALSE) : 
#>   Caused by error in `abort_if_not()`.
#> ℹ In argument: `x`.
#> ! Returned `FALSE`.
# Error in `myfunc()`:
# Caused by error in `abort_if_not()`:
# ℹ In argument: `x`.
# ! Returned `FALSE`.

# abort_if() errors if any argument does not evaluate to
# (all) FALSE:
abort_if(1 == 1) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `abort_if()`.
#> ℹ In argument: `1 == 1`.
#> ! Returned `TRUE`.
# Error:
# Caused by error in `abort_if()`:
# ℹ In argument: `1 == 1`.
# ! Returned `TRUE`.

# Injection can be used:
x <- "my error"
abort_if_not({{ x }} := FALSE) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `abort_if_not()`.
#> ℹ In argument: `FALSE`.
#> ! my error
abort_if_not(!!x := FALSE) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `abort_if_not()`.
#> ℹ In argument: `FALSE`.
#> ! my error
abort_if_not(FALSE, .message = "{x}") |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `abort_if_not()`.
#> ℹ In argument: `FALSE`.
#> ! my error
# Error:
# Caused by error in `abort_if_not()`:
# ℹ In argument: `FALSE`.
# ! my error
x <- list("my {.var bang-bang-bang} error" = FALSE)
abort_if_not(!!!x) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `abort_if_not()`.
#> ℹ In argument: `FALSE`.
#> ! my `bang-bang-bang` error
# Error:
# Caused by error in `abort_if_not()`:
# ℹ In argument: `FALSE`.
# ! my `bang-bang-bang` error
```
