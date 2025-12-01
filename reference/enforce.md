# Ensure the truth of R expressions and cast/recycle objects.

If any of the expressions in `...` are not all `TRUE`,
[abort](https://rlang.r-lib.org/reference/abort.html) is called for the
first which was not ([all](https://rdrr.io/r/base/all.html)) `TRUE`.
Alternatively, [rlang
formulas](https://rlang.r-lib.org/reference/new_formula.html) can be
used to pass multiple objects to validation formulas/functions, and/or
attempt safe type casting and size recycling using the
[cast](https://lj-jenkins.github.io/favr/reference/favr_casting_recycling_helpers.md),
[recycle](https://lj-jenkins.github.io/favr/reference/favr_casting_recycling_helpers.md)
and
[coerce](https://lj-jenkins.github.io/favr/reference/favr_casting_recycling_helpers.md)
functions. The rhs of formulas can be given in a
[list](https://rdrr.io/r/base/list.html) to pass multiple
functions/formulas/calls. Expressions are evaluated in the environment
specified and objects are assigned back into that environment. Type
casting and recycling are undertaken using the
[vctrs](https://vctrs.r-lib.org/) package and thus apply [vctrs type and
size rules](https://vctrs.r-lib.org/articles/type-size.html).

## Usage

``` r
enforce(..., .env = caller_env(), .error_call = caller_env())
```

## Arguments

- ...:

  any number of R expressions or formulas to be evaluated. Expressions
  must evaluate to logical whilst formulas can use
  [c](https://rdrr.io/r/base/c.html) on the lhs and either functions or
  formulas that evaluate to logical, or one of the type/size functions:
  [cast](https://lj-jenkins.github.io/favr/reference/favr_casting_recycling_helpers.md),
  [recycle](https://lj-jenkins.github.io/favr/reference/favr_casting_recycling_helpers.md)
  or
  [coerce](https://lj-jenkins.github.io/favr/reference/favr_casting_recycling_helpers.md)
  on the rhs. The rhs of a formula can also be a
  [list](https://rdrr.io/r/base/list.html) of multiple
  functions/formulas/calls. If an expression is named, or if the list
  element on the rhs of a formula is named, the name is passed to
  [format_inline](https://cli.r-lib.org/reference/format_inline.html)
  and is used in the error message.

- .env:

  the environment to use for the evaluation of the expressions and the
  assignment of the objects.

- .error_call:

  the call environment to use for error messages (passed to
  [abort](https://rlang.r-lib.org/reference/abort.html)).

## Value

NULL, but objects casted/recycled in `...` will be changed in the `.env`
environment specified.

## Details

See
[abort_if_not](https://lj-jenkins.github.io/favr/reference/abort_if_not.md)
for only validations and
[schema](https://lj-jenkins.github.io/favr/reference/schema.md) for a
data-masked version of this function.

## Examples

``` r
# NB: Some of these examples are expected to produce an error. To
#     prevent them from terminating a run with example() they are
#     piped into a call to try().

x <- 1L
y <- "hi"
z <- \(x) x > 1
enforce(x == 1, is.character(y), is.function(z)) # all TRUE

enforce(x == 2) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `enforce()`.
#> ℹ In argument: `x == 2`.
#> ! Returned `FALSE`.

# A custom error message can be given for each expression by
# naming it:
enforce(
  "{.var y} must be {.cls numeric}, check input" = is.numeric(y)
) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `enforce()`.
#> ℹ In argument: `is.numeric(y)`.
#> ! `y` must be <numeric>, check input

# Formulas can be used to take pass multiple objects
# on the lhs, with functions/additional formulas required on
# the rhs:
enforce(
  "multiple objects using: {.fn c}" = c(x, y) ~ is.integer
) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `enforce()`.
#> ℹ For named element: `y`.
#> ℹ In argument: `c(x, y) ~ is.integer`.
#> ! multiple objects using: `c()`

# Formulas can also be used with `cast()`, `recycle()`, and
# `coerce()` on the rhs to safely cast or recycle objects:
enforce(x ~ cast(double()))
class(x) # x is now numeric
#> [1] "numeric"
enforce(x ~ recycle(5))
length(x) # x is now length 5
#> [1] 5
enforce(y ~ coerce(type = factor(), size = 5))
print(y) # y is now factor and length 5
#> [1] hi hi hi hi hi
#> Levels: hi

# Multiple calls can be used with formulas by wrapping them
# in `list()`, with the names of list elements being
# preferentially chosen for error messaging and the error
# message also showing which formula/function/call caused the
# error:
enforce(
  "generic message" = c(x, y, z) ~ list(
    Negate(is.null),
    "{.var specific} message" = Negate(is.function)
  )
) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `enforce()`.
#> ℹ For named element: `z`.
#> ℹ In argument: `c(x, y, z) ~ list(... Negate(is.function) ...)`.
#> ! `specific` message

# Changed elements are available immediately:
x <- y <- 1L
enforce(x ~ cast(double()), y ~ cast(x))
cat(class(x), class(y)) # both now numeric
#> numeric numeric

# The `.error_call` argument can be used to specify where the
# error occurs, by default this is the caller environment:
myfunc <- function(...) enforce(...)
myfunc(x > 4) |> try()
#> Error in myfunc(x > 4) : Caused by error in `enforce()`.
#> ℹ In argument: `x > 4`.
#> ! Returned `FALSE`.

# rlang injection can be used:
msg <- "{.var injection} msg"
cols <- quote(c(x, y))
enforce(!!msg := !!cols ~ is.integer) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `enforce()`.
#> ℹ For named element: `x`.
#> ℹ In argument: `c(x, y) ~ is.integer`.
#> ! `injection` msg

# Objects are reverted to their original values if an error
# occur:
x <- y <- 1L
enforce(
  x ~ cast(double()), y ~ recycle(5), y ~ is.function
) |> try() # errors
#> Error in eval(expr, envir) : 
#>   Caused by error in `enforce()`.
#> ℹ In argument: `y ~ is.function`.
#> ! Returned `FALSE`.
class(x) # integer
#> [1] "integer"
length(y) # 1
#> [1] 1
```
