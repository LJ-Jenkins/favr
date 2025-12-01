# Ensure the truth of data-masked R expressions and cast/recycle named elements.

If any of the expressions in `...`, evaluated within the data mask
`'data'` (see [data
masking](https://rlang.r-lib.org/reference/args_data_masking.html)), are
not all `TRUE`, [abort](https://rlang.r-lib.org/reference/abort.html) is
called for the first which was not
([all](https://rdrr.io/r/base/all.html)) `TRUE`. Alternatively, [rlang
formulas](https://rlang.r-lib.org/reference/new_formula.html) can be
used to take advantage of
[tidyselect](https://tidyselect.r-lib.org/index.html) features and pass
multiple named elements in `data` to validation formulas/functions,
and/or attempt safe type casting and size recycling using the
[cast](https://lj-jenkins.github.io/favr/reference/favr_casting_recycling_helpers.md),
[recycle](https://lj-jenkins.github.io/favr/reference/favr_casting_recycling_helpers.md)
and
[coerce](https://lj-jenkins.github.io/favr/reference/favr_casting_recycling_helpers.md)
functions. The rhs of formulas can be given in a
[list](https://rdrr.io/r/base/list.html) to pass multiple
functions/formulas/calls. The `.names` and `.size` arguments can also be
used to check for given names and size of the data.frame/list itself.
Type casting, size checking, and recycling are undertaken using the
[vctrs](https://vctrs.r-lib.org/) package and thus apply [vctrs type and
size rules](https://vctrs.r-lib.org/articles/type-size.html).

## Usage

``` r
schema(data, ...)

# S3 method for class 'list'
schema(
  data,
  ...,
  .names = NULL,
  .size = NULL,
  .error_call = caller_env(),
  .darg = caller_arg(data)
)

# S3 method for class 'data.frame'
schema(
  data,
  ...,
  .names = NULL,
  .size = NULL,
  .error_call = caller_env(),
  .darg = caller_arg(data)
)

enforce_schema(data, ...)

# S3 method for class 'with_schema'
enforce_schema(data, ..., .error_call = caller_env(), .darg = caller_arg(data))

add_to_schema(data, ...)

# S3 method for class 'with_schema'
add_to_schema(
  data,
  ...,
  .names = NULL,
  .size = NULL,
  .error_call = caller_env(),
  .darg = caller_arg(data)
)
```

## Arguments

- data:

  a data.frame or list to use as the data mask.

- ...:

  any number of R expressions or formulas to be evaluated using `data`
  as a data mask. Formulas can use tidyselect syntax on the lhs and
  either functions or formulas that evaluate to logical, or one of the
  type/size functions:
  [cast](https://lj-jenkins.github.io/favr/reference/favr_casting_recycling_helpers.md),
  [recycle](https://lj-jenkins.github.io/favr/reference/favr_casting_recycling_helpers.md)
  and
  [coerce](https://lj-jenkins.github.io/favr/reference/favr_casting_recycling_helpers.md)
  on the rhs. The rhs of a formula can also be a
  [list](https://rdrr.io/r/base/list.html) of multiple
  functions/formulas/calls. If an expression is named, or if the list
  element on the rhs of a formula is named, the name is passed to
  [format_inline](https://cli.r-lib.org/reference/format_inline.html)
  and is used in the error message.

- .names:

  character vector of names which must be present in the `data`
  data.frame/list.

- .size:

  positive scalar integerish value for the size that the `data`
  data.frame/list must be.

- .error_call:

  the call environment to use for error messages (passed to
  [abort](https://rlang.r-lib.org/reference/abort.html)).

- .darg:

  the argument name of `data` to use in error messages.

## Value

`data` is returned with attached class `with_schema` and attribute
`schema` containing the schema call to be enforced later.

## Details

See
[abort_if_not](https://lj-jenkins.github.io/favr/reference/abort_if_not.md)
for a non-data-masked validation tool and
[enforce](https://lj-jenkins.github.io/favr/reference/enforce.md) for a
non-data-masked version of this function.

## Examples

``` r
# NB: Some of these examples are expected to produce an error. To
#     prevent them from terminating a run with example() they are
#     piped into a call to try().

li <- list(x = 1L, y = "hi", z = \(x) x > 1)
li <- li |>
  schema(x == 1, is.character(y), is.function(z)) # all TRUE

# The schema call is attached to the returned object and
# can be re-evaluated using enforce_schema():
li <- enforce_schema(li) # no error
li2 <- li
li2$x <- 2L
enforce_schema(li2) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `enforce_schema()`.
#> ℹ In argument: `x == 1`.
#> ! Returned `FALSE`.

# Calling `schema()` again overwrites any existing schema.
# Alternatively use `add_to_schema()` to add arguments to
# an existing schema (.size overwrites, other args append):
li <- li |>
  add_to_schema(is.numeric(x), .names = c("x", "y"), .size = 3)

# A custom error message can be given for each expression by
# naming it:
schema(li,
  "{.var y} must be {.cls numeric}, check input" = is.numeric(y)
) |> try()
#> Error in eval(expr, envir) : Caused by error in `schema()`.
#> ℹ In argument: `is.numeric(y)`.
#> ! `y` must be <numeric>, check input

# Formulas can be used to take advantage of tidyselect features
# on the lhs, with functions/additional formulas required on
# the rhs:
schema(li,
  "multiple columns: {.pkg tidyselect}" = c(x, y) ~ is.integer
) |> try()
#> Error in eval(expr, envir) : Caused by error in `schema()`.
#> ℹ For named element: `y`.
#> ℹ In argument: `c(x, y) ~ is.integer`.
#> ! multiple columns: tidyselect

# Formulas can also be used with `cast()`, `recycle()`, and
# `coerce()` on the rhs to safely cast or recycle named
# elements:
class(schema(li, x ~ cast(double()))$x) # x is now numeric
#> [1] "numeric"
length(schema(li, x ~ recycle(5))$x) # x is now length 5
#> [1] 5
schema(
  li,
  y ~ coerce(type = factor(), size = 5)
)$y # y is now factor and length 5
#> [1] hi hi hi hi hi
#> Levels: hi

# Multiple calls can be used with formulas by wrapping them
# in `list()`, with the names of list elements being
# preferentially chosen for error messaging and the error
# message also showing which formula/function/call caused the
# error:
schema(
  li,
  "generic message" = c(x, y, z) ~ list(
    Negate(is.null),
    "{.var specific} message" = Negate(is.function)
  )
) |> try()
#> Error in eval(expr, envir) : Caused by error in `schema()`.
#> ℹ For named element: `z`.
#> ℹ In argument: `c(x, y, z) ~ list(... Negate(is.function) ...)`.
#> ! `specific` message

# Changed elements are available immediately:
df <- data.frame(x = 1L, y = 1L)
lapply(schema(df, x ~ cast(double()), y ~ cast(x)), class)
#> $x
#> [1] "numeric"
#> 
#> $y
#> [1] "numeric"
#> 
# both now numeric

# `.names` and `.size` arguments can be used to check that given
# names are present and that the data has the desired size:
schema(li, .names = c("a", "x", "y", "b")) |> try()
#> Error in eval(expr, envir) : Caused by error in `schema()`.
#> ! Named elements `a` and `b` not found in data mask `li`.
schema(li, .size = 5) |> try()
#> Error in eval(expr, envir) : Caused by error in `schema()`.
#> ! Object `li` is of vctrs size `3`, not `5`.

# The `.error_call` argument can be used to specify where the
# error occurs, by default this is the caller environment:
myfunc <- function(x, ...) schema(x, ...)
myfunc(li, x > 4) |> try()
#> Error in myfunc(li, x > 4) : Caused by error in `schema()`.
#> ℹ In argument: `x > 4`.
#> ! Returned `FALSE`.

# rlang pronouns and injection can be used, but care must be
# taken when using `.env` and `enforce_schema()` as the
# caller environment may have changed:
msg <- "{.var injection} msg"
cols <- quote(c(x, y))
schema(li, !!msg := !!cols ~ is.integer) |> try()
#> Error in eval(expr, envir) : Caused by error in `schema()`.
#> ℹ For named element: `y`.
#> ℹ In argument: `c(x, y) ~ is.integer`.
#> ! `injection` msg

x <- 1L
li <- schema(li, x == .env$x) # no error

x <- 2
enforce_schema(li) |>
  try() # error as the environmental variable has changed
#> Error in eval(expr, envir) : 
#>   Caused by error in `enforce_schema()`.
#> ℹ In argument: `x == .env$x`.
#> ! Returned `FALSE`.
```
