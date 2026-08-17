# walk_check() errors on non-vector input

    Code
      walk_check(e, is.numeric)
    Condition
      Error:
      ! `.x` must be a <vector>, not an <environment>.
    Code
      walk_check(mean, is.numeric)
    Condition
      Error:
      ! `.x` must be a <vector>, not a <function>.

# walk_check() allows function, formula or string

    Code
      walk_check(list(1, 2, 3), 1)
    Condition
      Error in `walk_check()`:
      ! Can't convert `.f`, a double vector, to a function.

# walk_check() errors on non-logical returns

    Code
      walk_check(list(1, 2, 3), ~"not logical")
    Condition
      Error:
      ! Check result for `.x[[1]]` must be a <logical> vector, not the string "not logical".

# walk_check() errors on NA returns

    Code
      walk_check(list(1, 2, 3), ~NA)
    Condition
      Error:
      ! Check result for `.x[[1]]` is NA.
    Code
      walk_check(list(1, 2, 3), ~ c(TRUE, NA))
    Condition
      Error:
      ! Check result for `.x[[1]]` contains NA values.

# walk_check() errors when check fails

    Code
      walk_check(list(1, 2, 3), ~ .x < 3)
    Condition
      Error:
      ! Check result for `.x[[3]]` is not TRUE.

# walk_check() errors show index and name (if present)

    Code
      walk_check(list(1, 2, my_named_element = 3), ~ .x < 3)
    Condition
      Error:
      ! Check result for `.x[['my_named_element']]` (index: 3) is not TRUE.

