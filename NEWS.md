# favr 2.0.0 (in development)

Refactor of the package to focus on function argument validation through "check" functions that throw an error if validation fails and return `NULL` (or occasionally the input) invisibly if validation succeeds.

## New Features

### General Validation

* `abortif()` and `abortifnot()` functions to check conditions and error if a condition is `TRUE` or `FALSE`, respectively.
* `check()` function to check conditions using tidy eval and error if a condition is `TRUE`.
* `check_with()` function to check conditions in a data-masked context and error if a condition is `TRUE`.
* `walk_check()` function to walk a predicate over a vector and error if any element fails the check.

### Type and Class Validation

* `check_inherits()` and `check_class()` functions to check the class of an object and error if it does not inherit from a specified class.

### Type Validation

* `check_list()`, `check_atomic()`, `check_vector()`, `check_integer()`, `check_double()`, `check_numeric()`, `check_character()`, `check_logical()`, `check_complex()`, `check_raw()`, `check_bytes()` and `check_null()` functions to check the type of an object and error if it does not match the specified type.

* `check_scalar_list()`, `check_scalar_atomic()`, `check_scalar_vector()`, `check_scalar_integer()`, `check_scalar_double()`, `check_scalar_numeric()`, `check_scalar_character()`, `check_scalar_logical()`, `check_scalar_complex()`, `check_scalar_raw()` and `check_scalar_bytes()` functions to check if an object is a scalar of the specified type and error if it is not.

* `check_array()` and `check_matrix()` functions to check if an object is an array or matrix, respectively, and error if it is not.

### S3 Type Validation

* `check_table()`, `check_factor()`, `check_ordered()`, `check_date()`, `check_posixct()` and `check_posixlt()` functions to check if an object is of the specified S3 vector type and error if it is not.

* `check_data_frame()`, `check_tibble()`, `check_data_table()` and `check_tidytable()` functions to check if an object is of the specified S3 data frame type and error if it is not.

* `check_vctr()` and `check_list_of()` functions to check if an object is of the specified `vctrs` S3  type and error if it is not.

* `check_s3_vec()` and `check_s3_df()` functions for developers to create their own S3 type checks for vector and data frame types, respectively.

### Type Checking Modifiers

* `bare()` modifier to check for bare objects (i.e. objects with no class attribute) in the type check functions, or bare S3 objects (where the expected S3 class is first in the class attribute vector) in the S3 type check functions.

* `at_least()`, `at_most()`, and `in_range()` modifiers to check for ranges in length/number of rows/number of columns.

### Scalar Value Validation

* `check_true()`, `check_false()`, `check_bool()` and `check_string()` functions to check if an object is a scalar of the specified type and value and error if it is not.

### File and Directory Validation

* `check_dir()` and `check_file()` functions to check if a directory or file exists and error if it does not.
* `check_ext()` function to check the file extension of a file and error if it does not match the specified extensions.

## Questioning

Removed the online documentation for `enforce()`, `schema()`, `add_to_schema()`, `enforce_schema()`, and the associated casting and recycling helpers (`cast()`, `lossy()`, `recycle()` and `coerce()`).

## Soft-Deprecated

* `abort_if_not()` should be replaced with `abortifnot()` or `check()`.
* `cast_if_not()` and `recycle_if_not()` should be replaced with their `vctrs` equivalents, `vctrs::vec_cast()` and `vctrs::vec_recycle()`, respectively.
* All `are_*()` functions and the `have_names()` function - their use cases were limited and are easily replicated using `base::vapply()` and the associated predicate function.

## Bug Fixes

* Fixed a bug where `are_scalar_integerish()` would immediately error due to an erroneous `.n` argument.

# favr 1.0.0

* Added a `NEWS.md` file to track changes to the package.
