
<!-- README.md is generated from README.Rmd. Please edit that file -->

# favr

Function Argument Validation tools for R (favr) provides tools for the
validation and safe type coercion/recycling of function arguments. A
focus is placed on clear error messaging.

## Overview

- `abort_if_not()` for general validation.
- `cast_if_not()` and `recycle_if_not()` for safe type casting and
  recycling of variables.
- `enforce()` for validation and safe type casting and recycling of
  variables.
- `schema()` for the validation and safe type casting and recycling of
  named elements of data.frames/lists.
- `enforce_schema()` to re-evaluate a prior schema call that was
  attached to the data.frame/list.
- `add_to_schema()` add arguments to an existing attached schema and
  re-evaluate.

## Installation

You can install the development version of favr from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("LJ-Jenkins/favr")
```

## Examples

`abort_if_not` can be used for all validations:

``` r
library(favr, warn.conflicts = FALSE)

f <- \(x, y) {
  abort_if_not(
    is.character(x),
    "`{x}` is too short!" = nchar(x) > 5,
    y$x == 1
  )
}

f(1L, list(x = 1))
#> Error in `f()`:
#> Caused by error in `abort_if_not()`.
#> ℹ In argument: `is.character(x)`.
#> ! Returned `FALSE`.

f("hi", list(x = 1))
#> Error in `f()`:
#> Caused by error in `abort_if_not()`.
#> ℹ In argument: `nchar(x) > 5`.
#> ! `hi` is too short!
```

`cast_if_not` and `recycle_if_not` provide safe casting and recycling
from [vctrs](https://vctrs.r-lib.org/). Variables are provided on the
left hand side and the expected type/size is provided on the right.
Assignment is automatically done back into the environment specified
(default is the
[caller_env()](https://rlang.r-lib.org/reference/stack.html)):

``` r
x <- 5L
y <- 1

cast_if_not(x = double())
recycle_if_not(y = x)

class(x)
#> [1] "numeric"
length(y)
#> [1] 5

x <- 1.5

cast_if_not(x = lossy(integer()))

class(x)
#> [1] "integer"

x <- "hi"

cast_if_not(x = integer())
#> Error:
#> Caused by error in `cast_if_not()`.
#> ℹ In argument: `x = integer()`.
#> ! Can't convert `x` <character> to <integer>.
```

`enforce` allows both validations, casting and recycling using the
keyword functions of `cast`, `recycle` and `coerce`. Formulas needs to
be used for non-validations. For formulas, `c()` can be used to pass
multiple objects to a specific validation/call and multiple arguments
can be given on the rhs when wrapped in `list()`. Assignment occurs back
into the environment specified (default is the
[caller_env()](https://rlang.r-lib.org/reference/stack.html)).

``` r
li <- list(x = 1.5)
y <- 1.5

enforce(
  "{.var li} problem" = li ~ list(
    \(.x) names(.x) == "x",
    coerce(type = list(x = integer()), size = 3, lossy = TRUE),
    "list element not 1?" = ~ length(.x$x) == 1,
    "list itself now length 3" = ~ length(.x) == 3
  ),
  "{.var y} below zero" = y > 0,
  y ~ recycle(10)
)

class(li$x)
#> [1] "numeric"
length(li)
#> [1] 3
length(y)
#> [1] 10

#-- vctrs type/size rules are for all `cast`, `recycle` and `coerce` calls within favr functions

df <- data.frame(x = 1L, y = "hi")

enforce(df ~ cast(data.frame(x = integer(), y = double())))
#> Error:
#> Caused by error in `enforce()`.
#> ℹ In argument: `df ~ cast(data.frame(x = integer(), y = double()))`.
#> ! Can't convert `df$y` <character> to match type of `y` <double>.

x <- 1
y <- 1:5

enforce(c(x, y) ~ list(~ .x > 0, recycle(10)))
#> Error:
#> Caused by error in `enforce()`.
#> ℹ In argument: `c(x, y) ~ list(... recycle(10) ...)`.
#> ! Can't recycle `y` (size 5) to size 10.
```

`schema` provide the same functionality for data-masked arguments from
data.frames/lists. The size of the data.frame/list and whether certain
names are present can also be checked using the `.names` and `.size`
arguments. The altered data-mask object is returned with an attached
class `with_schema` which is used by `add_to_schema()` and
`enforce_schema()` to edit and/or re-evaluate the original schema call.
Tidyselect syntax can be used on the lhs of formulas.

``` r
data.frame(x = 2) |>
  schema(x == 1)
#> Error:
#> Caused by error in `schema()`.
#> ℹ In argument: `x == 1`.
#> ! Returned `FALSE`.

data.frame(x = 1L) |>
  schema(x ~ cast(double())) |>
  (\(.) class(.$x))()
#> [1] "numeric"

# recycling is only implemented for lists.
list(x = 1, y = 1, z = 1) |>
  schema(
    x ~ recycle(3),
    y ~ recycle(5),
    z ~ recycle(vctrs::vec_size(x))
  ) |>
  lengths()
#> x y z 
#> 3 5 3

# enforce_schema reapplies the original call.
li <- list(x = 1, y = "hi")
li_with_schema <- schema(li, x == 1, is.character(y))
li_with_schema$y <- 1

enforce_schema(li_with_schema)
#> Error:
#> Caused by error in `enforce_schema()`.
#> ℹ In argument: `is.character(y)`.
#> ! Returned `FALSE`.

df <- data.frame(x = 1:2, xx = 3:4)
df_with_schema <- schema(df, starts_with("x") ~ cast(integer(), lossy = TRUE))
df_with_schema$x <- c(1.5, 2.5)

enforce_schema(df_with_schema)$x
#> [1] 1 2

li_with_schema <- schema(li, c(x, y) ~ recycle(3))
li_with_schema$y <- "hi"

enforce_schema(li_with_schema)$y
#> [1] "hi" "hi" "hi"

# add_to_schema adds to an existing schema and then re-evaluates.
li_with_schema <- li_with_schema |>
  add_to_schema(.names = c("x", "y"), .size = 2)

li_with_schema <- li_with_schema |>
  add_to_schema(y ~ \(.x) nchar(.x) > 2)
#> Error:
#> Caused by error in `add_to_schema()`.
#> ℹ For named element: `y`.
#> ℹ In argument: `y ~ function(.x) nchar(.x) > 2`.
#> ! Returned `FALSE`.
```

### Notes

favr functions that assign into environments (`cast_if_not`,
`recycle_if_not`, and `enforce`) all do clean-up when errors occur:

``` r
x <- 1L
y <- 1L
cast_if_not(x = double(), y = character()) |> try()
#> Error in (function (...)  : Caused by error in `cast_if_not()`.
#> ℹ In argument: `y = character()`.
#> ! Can't convert `y` <integer> to <character>.
cat("Code has errored but `x` has reverted back to:", class(x))
#> Code has errored but `x` has reverted back to: integer
```

favr was inspired by MATLAB’s [arguments
block](https://uk.mathworks.com/help/matlab/ref/arguments.html) and
[schematic](https://whipson.github.io/schematic/). favr relies heavily
on the imported packages [rlang](https://rlang.r-lib.org),
[vctrs](https://vctrs.r-lib.org/), [cli](https://cli.r-lib.org/) and
[tidyselect](https://tidyselect.r-lib.org/). For function argument
validation that focus on performance, see
[checkmate](https://mllg.github.io/checkmate/).
