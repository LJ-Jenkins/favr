# length modifiers error for non-castable objects

    Code
      at_least(mean)
    Condition
      Error in `at_least()`:
      ! `mean` must be a vector, not a function.
      i Read our FAQ about scalar types (`?vctrs::faq_error_scalar_type`) to learn more.
    Code
      at_most(1.5)
    Condition
      Error in `at_most()`:
      ! Can't convert from `1.5` <double> to `at_most 'n'` <integer> due to loss of precision.
      * Locations: 1
    Code
      in_range("a", "b")
    Condition
      Error in `in_range()`:
      ! Can't convert `c("a", "b")` <character> to match type of `in range 'n'` <integer>.

# length modifiers error for non-scalars

    Code
      at_least(c(1, 2))
    Condition
      Error in `at_least()`:
      ! `c(1, 2)` must be a single numeric value, but is of length 2.
    Code
      at_most(c(1, 2))
    Condition
      Error in `at_most()`:
      ! `c(1, 2)` must be a single numeric value, but is of length 2.
    Code
      in_range(c(1, 2), c(3, 4))
    Condition
      Error in `in_range()`:
      ! `n_min` and `n_max` must be single numeric values, but combined are of length 4.

# length modifiers error for negative values

    Code
      at_least(-1)
    Condition
      Error in `at_least()`:
      ! `-1` must be greater than or equal to zero, but -1 was provided.
    Code
      at_most(-1)
    Condition
      Error in `at_most()`:
      ! `-1` must be greater than or equal to zero, but -1 was provided.
    Code
      in_range(-1, 2)
    Condition
      Error in `in_range()`:
      ! `n_min` must be greater than or equal to zero, but -1 was provided.

# in_range errors if n_max bigger than n_min

    Code
      in_range(2, 1)
    Condition
      Error in `in_range()`:
      ! `c(2, 1)` must be a valid range in the form of `c(n_min, n_max)`, but `n[1]` 2 is greater than `n[2]` 1.

# bare modifier correctly errors for non-bare objects

    Code
      check_integer(bare(factor(1)))
    Condition
      Error:
      ! `factor(1)` must be a bare <integer>, but it is of class <factor>.

---

    Code
      check_date(bare(x))
    Condition
      Error:
      ! `x` must be a bare <Date>, but it is of class <my_date>.

---

    Code
      check_data_frame(bare(x))
    Condition
      Error:
      ! `x` must be a bare <data.frame>, but it is of class <my_df>.

# length modifiers correctly control n length checks

    Code
      check_atomic(1:5, n = at_least(10))
    Condition
      Error:
      ! `1:5` must be an <atomic> vector of at least length 10, but it is of length 5.
    Code
      check_atomic(1:5, n = at_most(3))
    Condition
      Error:
      ! `1:5` must be an <atomic> vector of at most length 3, but it is of length 5.
    Code
      check_atomic(1:5, n = in_range(6, 10))
    Condition
      Error:
      ! `1:5` must be an <atomic> vector of a length between 6 and 10, but it is of length 5.
    Code
      check_atomic(1:5, n = in_range(2, 4))
    Condition
      Error:
      ! `1:5` must be an <atomic> vector of a length between 2 and 4, but it is of length 5.

