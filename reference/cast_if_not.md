# Cast objects to a given type

The names of the `...` expressions, which should be variables within the
`.env` envrionment, are attempted to be casted to the type specified in
the expression: e.g.,
`name_of_object_to_cast = object_of_type_to_cast_to`. Expressions are
evaluated in the environment specified and objects are assigned back
into that same environment. Lossy casting can be undertaken by wrapping
the expression in a call to
[lossy](https://lj-jenkins.github.io/favr/reference/favr_casting_recycling_helpers.md),
e.g., `x = lossy(integer())`. The type conversion is from the
[vctrs](https://vctrs.r-lib.org/) package and thus sticks to the [vctrs
type conversion
rules](https://vctrs.r-lib.org/reference/faq-compatibility-types.html).

## Usage

``` r
cast_if_not(..., .env = caller_env(), .error_call = caller_env())
```

## Arguments

- ...:

  any number of named R expressions.

- .env:

  the environment to use for the evaluation of the casting expressions
  and the assignment of the casted objects.

- .error_call:

  the call environment to use for error messages (passed to
  [abort](https://rlang.r-lib.org/reference/abort.html)).

## Value

NULL, but objects named in `...` will be changed in the `.env`
environment specified.

## Details

See
[abort_if_not](https://lj-jenkins.github.io/favr/reference/abort_if_not.md)
for general validation,
[recycle_if_not](https://lj-jenkins.github.io/favr/reference/recycle_if_not.md)
for recycling, and
[enforce](https://lj-jenkins.github.io/favr/reference/enforce.md) and
[schema](https://lj-jenkins.github.io/favr/reference/schema.md) for non
data-masked and data-masked validations, recycling and casting.

## Examples

``` r
# NB: Some of these examples are expected to produce an error. To
#     prevent them from terminating a run with example() they are
#     piped into a call to try().

x <- 1L
cast_if_not(x = double())
class(x) # numeric
#> [1] "numeric"

# By default, lossy casting is not allowed:
x <- c(1, 1.5)
cast_if_not(x = integer()) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `cast_if_not()`.
#> ℹ In argument: `x = integer()`.
#> ! Can't convert from `x` <double> to <integer> due to loss of precision.
#> • Locations: 2

# lossy casting can be enabled using `lossy()` call:
cast_if_not(x = lossy(integer()))
class(x) # integer
#> [1] "integer"

# Other objects can be used as the type to cast to, e.g.:
x <- 1L
y <- 2.3
cast_if_not(x = y)
class(x) # numeric
#> [1] "numeric"

# Changed objects are available immediately:
x <- y <- 1L
cast_if_not(x = double(), y = x)
cat(class(x), class(y), sep = ", ") # numeric, numeric
#> numeric, numeric

myfunc <- function(x) {
  cast_if_not(x = double())
  class(x)
}
x <- 1L
myfunc(x) # x is cast to double within the function
#> [1] "numeric"
class(x) # x is still an integer outside the function
#> [1] "integer"

# The `.env` argument determines the expression and assignment
# environment:
x <- 1L
e <- new.env()
e$x <- 1L
cast_if_not(x = 1.5, .env = e)
cat(
  "environment 'e'", class(e$x), "local environment", class(x),
  sep = ", "
) # environment 'e', numeric, local environment, integer
#> environment 'e', numeric, local environment, integer

# Named objects (lhs) are checked to be in the `.env` environment,
# throwing an error if not found:
x <- 1L
e <- new.env()
cast_if_not(x = 1.5, .env = e) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `cast_if_not()`.
#> ! Object `x` is not found in the `.env` environment specified.

# For expressions (rhs), the `.env` argument is preferentially
# chosen, but if not found then the normal R scoping rules
# apply:
x <- 1.5
e <- new.env()
e$z <- 1L
cast_if_not(z = x, .env = e)
class(e$z) # numeric
#> [1] "numeric"

# The `.error_call` argument can be used to specify where the
# error occurs, by default this is the caller environment:
myfunc <- function(x) cast_if_not(x = character())
myfunc(FALSE) |> try()
#> Error in myfunc(FALSE) : 
#>   Caused by error in `cast_if_not()`.
#> ℹ In argument: `x = character()`.
#> ! Can't convert `x` <logical> to <character>.

# Injection can be used:
y <- 1L
x <- "y"
cast_if_not(!!x := double()) |> try()
class(y) # numeric
#> [1] "numeric"

y <- 1L
x <- list(y = double())
cast_if_not(!!!x)
class(y) # numeric
#> [1] "numeric"

# Objects are reverted to their original values if an error
# occur:
x <- y <- 1L
cast_if_not(x = double(), y = character()) |> try()
#> Error in eval(expr, envir) : 
#>   Caused by error in `cast_if_not()`.
#> ℹ In argument: `y = character()`.
#> ! Can't convert `y` <integer> to <character>.
class(x) # integer
#> [1] "integer"
```
