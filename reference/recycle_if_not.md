# Recycle objects to a given size

The names of the `...` expressions, which should be variables within the
`.env` envrionment, are attempted to be recycled to the size specified
in the expression: e.g.,
`name_of_object_to_recycle = size_to_recycle_to`. Expressions are
evaluated in the environment specified and objects are assigned back
into that same environment. The object recycling is from the
[vctrs](https://vctrs.r-lib.org/) package and thus stick to the [vctrs
recycling
rules](https://vctrs.r-lib.org/reference/theory-faq-recycling.html).

## Usage

``` r
recycle_if_not(..., .env = caller_env(), .error_call = caller_env())
```

## Arguments

- ...:

  any number of named R expressions.

- .env:

  the environment to use for the evaluation of the recycling expressions
  and the assignment of the recycled objects.

- .error_call:

  the call environment to use for error messages (passed to
  [abort](https://rlang.r-lib.org/reference/abort.html)).

## Value

NULL, but objects named in `...` will be changed in the `.env`
environment specified.

## Details

See
[abort_if_not](https://lj-jenkins.github.io/favr/reference/abort_if_not.md)
for general validation, recycle_if_not for recycling, and
[enforce](https://lj-jenkins.github.io/favr/reference/enforce.md) and
[schema](https://lj-jenkins.github.io/favr/reference/schema.md) for non
data-masked and data-masked validations, recycling and casting.

## Examples

``` r
# NB: Some of these examples are expected to produce an error. To
#     prevent them from terminating a run with example() they are
#     piped into a call to try().

x <- 1
recycle_if_not(x = 5)
length(x) # 5
#> [1] 5

# recycle_if_not() follows `vctrs` recycling rules:
x <- c(1, 1)
recycle_if_not(x = 6) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `recycle_if_not()`.
#> ℹ In argument: `x = 6`.
#> ! Can't recycle `x` (size 2) to size 6.

# Beware when using other objects as the size argument, e.g.:
x <- 1L
y <- c(1, 1, 1)
recycle_if_not(x = y) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `recycle_if_not()`.
#> ℹ In argument: `x = y`.
#> ! Size argument must be positive scalar integerish, not length `3`.

# When using other objects, call vctrs::vec_size() on them first:
recycle_if_not(x = vctrs::vec_size(y))
length(x) # 3
#> [1] 3

# Changed objects are available immediately:
x <- y <- 1
recycle_if_not(x = 3, y = vctrs::vec_size(x))
cat(length(x), length(y), sep = ", ") # 3, 3
#> 3, 3

myfunc <- function(x) {
  recycle_if_not(x = 3)
  length(x)
}
x <- 1L
myfunc(x) # x is recycled to length 3 within the function
#> [1] 3
length(x) # x is still scalar outside the function
#> [1] 1

# The `.env` argument determines the expression and assignment
# environment:
x <- 1
e <- new.env()
e$x <- 1
recycle_if_not(x = 3, .env = e)
cat(
  "environment 'e'", length(e$x), "local environment", length(x),
  sep = ", "
) # environment 'e', 3, local environment, 1
#> environment 'e', 3, local environment, 1

# Named objects (lhs) are checked to be in the `.env` environment,
# throwing an error if not found:
x <- 1
e <- new.env()
recycle_if_not(x = 3, .env = e) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `recycle_if_not()`.
#> ! Object `x` is not found in the `.env` environment specified.

# For expressions (rhs), the `.env` argument is preferentially
# chosen, but if not found then the normal R scoping rules
# apply:
x <- 3
e <- new.env()
e$z <- 1
recycle_if_not(z = x, .env = e)
length(e$z) # 3
#> [1] 3

# The `.error_call` argument can be used to specify where the
# error occurs, by default this is the caller environment:
myfunc <- function(x) recycle_if_not(x = -5)
myfunc(1) |> try()
#> Error in myfunc(1) : Caused by error in `recycle_if_not()`.
#> ℹ In argument: `x = -5`.
#> ! Size argument must be positive scalar integerish, not `-5`.

#' # Injection can be used:
y <- 1L
x <- "y"
recycle_if_not(!!x := 5) |> try()
length(y) # 5
#> [1] 5

y <- 1L
x <- list(y = 5)
recycle_if_not(!!!x)
length(y) # 5
#> [1] 5

# Objects are reverted to their original values if an error
# occur:
x <- y <- 1L
recycle_if_not(x = 5, y = -5) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `recycle_if_not()`.
#> ℹ In argument: `y = -5`.
#> ! Size argument must be positive scalar integerish, not `-5`.
length(x) # 1
#> [1] 1
```
