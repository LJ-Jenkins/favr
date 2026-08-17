# Check the truth of tidy evaluated expressions

If any of the expressions in `...` are not
[all](https://rdrr.io/r/base/all.html) `TRUE`,
[cli_abort()](https://cli.r-lib.org/reference/cli_abort.html) is called,
producing an error message indicating the first expression which was not
([all](https://rdrr.io/r/base/all.html)) `TRUE`.

`check_with()` is a data-masked version of `check()`, evaluating the
expression in the context of `.data`.

## Usage

``` r
check(
  ...,
  message = NULL,
  call = .envir,
  .envir = parent.frame(),
  .frame = .envir,
  abort_args = NULL
)

check_with(
  .data,
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
  [`TRUE`](https://rdrr.io/r/base/logical.html) for no error to occur.
  Non-`logical` and `NA` values will trigger an error.

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

  For `check_with()`, the messages are evaluated in the context of
  `.data` and `.envir`. See examples.

- .frame:

  The throwing context. Passed to
  [cli_abort()](https://cli.r-lib.org/reference/cli_abort.html).

- abort_args:

  A list of additional arguments to pass to
  [abort()](https://rlang.r-lib.org/reference/abort.html) (forwarded
  from [cli_abort()](https://cli.r-lib.org/reference/cli_abort.html)).

- .data:

  A data frame, list, or environment to evaluate the expressions in as a
  data mask.

## Value

`NULL`, called for side effects only.

## See also

[abortifnot](https://lj-jenkins.github.io/favr/reference/abortifnot.md)
for a more performant version without tidy evaluation and
[injection](https://rlang.r-lib.org/reference/topic-inject.html)
support.

## Examples

``` r
check(1 == 1, all.equal(pi, 3.14159265), 1 < 2) # all TRUE

data <- data.frame(x = 1:5, y = 6:10)
check_with(data, x < y, is.numeric(x), length(y) < 10) # all TRUE

# A custom error message can be given for each
# expression, with cli formatting.
check(
  "message {.arg 1}" = TRUE, "message {.arg 2}" = FALSE
) |> try()
#> Error in eval(expr, envir) : message `2`

# check_with() names are also are evaluated in
# the context of `.data` then `.envir`.
x <- "env 'x'"
y <- "env 'y'"
data <- list(x = "data 'x'")
check_with(data, "{x}" = is.numeric(x)) |>
  try()
#> Error in eval(expr, envir) : data 'x'
check_with(data, "{y}" = is.numeric(x)) |>
  try()
#> Error in eval(expr, envir) : env 'y'

# Pronouns are supported in check_with() error
# messages, but must be spaced according to cli
# rules (e.g., use `{ .env$x}` instead of `{.env$x}`).
check_with(data, "{ .env$x}" = is.numeric(x)) |>
  try()
#> Error in eval(expr, envir) : env 'x'

# Alternatively, one error message can be used for all
# expressions.
x <- 1:3
check(
  x > 0, x < 3,
  message = "{.arg x} has incorrect values: {.val {x}}."
) |> try()
#> Error in eval(expr, envir) : 
#>   `x` has incorrect values: 1, 2, and 3.

data <- data.frame(x = c("a", "b", "c"))
check_with(data,
  is.numeric(x),
  message = "{.arg x} is not numeric: {.val {x}}."
) |>
  try()
#> Error in eval(expr, envir) : 
#>   `x` is not numeric: "a", "b", and "c".

# The `call` argument can be used to specify where the
# error occurs, by default this is the caller environment.
myfunc <- function(x) check(x)
myfunc(FALSE) |> try()
#> Error in myfunc(FALSE) : `x` is not TRUE.

myfunc_with <- function(x, ...) check_with(x, ...)
myfunc_with(list(x = 1), x < 0) |> try()
#> Error in myfunc_with(list(x = 1), x < 0) : 
#>   `x < 0` is not TRUE.

# check() and check_with() error if any argument does
# not evaluate to (all) FALSE.
check(c(T, F)) |> try()
#> Error in eval(expr, envir) : `c(T, F)` is not TRUE.
check_with(list(x = c(T, NA)), x) |>
  try()
#> Error in eval(expr, envir) : 
#>   `x` must not contain NA values.
```
