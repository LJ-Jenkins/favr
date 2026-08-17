# Changelog

## favr 2.0.0

Refactor of the package to focus on function argument validation through
“check” functions that throw an error if validation fails and return
`NULL` (or occasionally the input) invisibly if validation succeeds.

### New Features

#### General Validation

- [`abortif()`](https://lj-jenkins.github.io/favr/reference/abortifnot.md)
  and
  [`abortifnot()`](https://lj-jenkins.github.io/favr/reference/abortifnot.md)
  functions to check conditions and error if a condition is `TRUE` or
  `FALSE`, respectively.
- [`check()`](https://lj-jenkins.github.io/favr/reference/check.md)
  function to check conditions using tidy eval and error if a condition
  is `TRUE`.
- [`check_with()`](https://lj-jenkins.github.io/favr/reference/check.md)
  function to check conditions in a data-masked context and error if a
  condition is `TRUE`.
- [`walk_check()`](https://lj-jenkins.github.io/favr/reference/walk-check.md)
  function to walk a predicate over a vector and error if any element
  fails the check.

#### Type and Class Validation

- [`check_inherits()`](https://lj-jenkins.github.io/favr/reference/inheritance-checks.md)
  and
  [`check_class()`](https://lj-jenkins.github.io/favr/reference/inheritance-checks.md)
  functions to check the class of an object and error if it does not
  inherit from a specified class.

#### Type Validation

- [`check_list()`](https://lj-jenkins.github.io/favr/reference/type-checks.md),
  [`check_atomic()`](https://lj-jenkins.github.io/favr/reference/type-checks.md),
  [`check_vector()`](https://lj-jenkins.github.io/favr/reference/type-checks.md),
  [`check_integer()`](https://lj-jenkins.github.io/favr/reference/type-checks.md),
  [`check_double()`](https://lj-jenkins.github.io/favr/reference/type-checks.md),
  [`check_numeric()`](https://lj-jenkins.github.io/favr/reference/type-checks.md),
  [`check_character()`](https://lj-jenkins.github.io/favr/reference/type-checks.md),
  [`check_logical()`](https://lj-jenkins.github.io/favr/reference/type-checks.md),
  [`check_complex()`](https://lj-jenkins.github.io/favr/reference/type-checks.md),
  [`check_raw()`](https://lj-jenkins.github.io/favr/reference/type-checks.md),
  [`check_bytes()`](https://lj-jenkins.github.io/favr/reference/type-checks.md)
  and
  [`check_null()`](https://lj-jenkins.github.io/favr/reference/type-checks.md)
  functions to check the type of an object and error if it does not
  match the specified type.

- [`check_scalar_list()`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md),
  [`check_scalar_atomic()`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md),
  [`check_scalar_vector()`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md),
  [`check_scalar_integer()`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md),
  [`check_scalar_double()`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md),
  [`check_scalar_numeric()`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md),
  [`check_scalar_character()`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md),
  [`check_scalar_logical()`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md),
  [`check_scalar_complex()`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md),
  [`check_scalar_raw()`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md)
  and
  [`check_scalar_bytes()`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md)
  functions to check if an object is a scalar of the specified type and
  error if it is not.

- [`check_array()`](https://lj-jenkins.github.io/favr/reference/array-type-checks.md),
  [`check_matrix()`](https://lj-jenkins.github.io/favr/reference/array-type-checks.md)
  and
  [`check_table()`](https://lj-jenkins.github.io/favr/reference/array-type-checks.md)
  functions to check if an object is an array, matrix, or table,
  respectively, and error if it is not.

#### S3 Type Validation

- [`check_factor()`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md),
  [`check_ordered()`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md),
  [`check_date()`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md),
  [`check_posixct()`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md)
  and
  [`check_posixlt()`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md)
  functions to check if an object is of the specified S3 vector type and
  error if it is not.

- [`check_data_frame()`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md),
  [`check_tibble()`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md),
  [`check_data_table()`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md)
  and
  [`check_tidytable()`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md)
  functions to check if an object is of the specified S3 data frame type
  and error if it is not.

- [`check_vctr()`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md)
  and
  [`check_list_of()`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md)
  functions to check if an object is of the specified `vctrs` S3 type
  and error if it is not.

- [`s3_vec_check()`](https://lj-jenkins.github.io/favr/reference/s3-check-builders.md)
  and
  [`s3_df_check()`](https://lj-jenkins.github.io/favr/reference/s3-check-builders.md)
  functions for developers to create their own S3 type checks for vector
  and data frame types, respectively.

#### Type Checking Modifiers

- [`bare()`](https://lj-jenkins.github.io/favr/reference/modifiers.md)
  modifier to check for bare objects (i.e. objects with no class
  attribute) in the type check functions, or bare S3 objects (where the
  expected S3 class is first in the class attribute vector) in the S3
  type check functions.

- [`at_least()`](https://lj-jenkins.github.io/favr/reference/modifiers.md),
  [`at_most()`](https://lj-jenkins.github.io/favr/reference/modifiers.md),
  and
  [`in_range()`](https://lj-jenkins.github.io/favr/reference/modifiers.md)
  modifiers to check for ranges in length/number of rows/number of
  columns.

#### Scalar Value Validation

- [`check_true()`](https://lj-jenkins.github.io/favr/reference/scalar-value-checks.md),
  [`check_false()`](https://lj-jenkins.github.io/favr/reference/scalar-value-checks.md),
  [`check_bool()`](https://lj-jenkins.github.io/favr/reference/scalar-value-checks.md)
  and
  [`check_string()`](https://lj-jenkins.github.io/favr/reference/scalar-value-checks.md)
  functions to check if an object is a scalar of the specified type and
  value and error if it is not.

#### Forbidden Values Validation

- [`check_no_na()`](https://lj-jenkins.github.io/favr/reference/forbidden-value-checks.md),
  [`check_finite()`](https://lj-jenkins.github.io/favr/reference/forbidden-value-checks.md)
  and
  [`check_nzchar()`](https://lj-jenkins.github.io/favr/reference/forbidden-value-checks.md)
  functions to check for the presence of forbidden values and error if
  found.

#### File and Directory Validation

- [`check_dir()`](https://lj-jenkins.github.io/favr/reference/path-checks.md)
  and
  [`check_file()`](https://lj-jenkins.github.io/favr/reference/path-checks.md)
  functions to check if a directory or file exists and error if it does
  not.
- [`check_ext()`](https://lj-jenkins.github.io/favr/reference/path-checks.md)
  function to check the file extension of a file/path and error if it
  does not match the specified extensions.

### Questioning

Removed the online documentation for
[`enforce()`](https://lj-jenkins.github.io/favr/reference/enforce.md),
[`schema()`](https://lj-jenkins.github.io/favr/reference/schema.md),
[`add_to_schema()`](https://lj-jenkins.github.io/favr/reference/schema.md),
[`enforce_schema()`](https://lj-jenkins.github.io/favr/reference/schema.md),
and the associated casting and recycling helpers
([`cast()`](https://lj-jenkins.github.io/favr/reference/favr_casting_recycling_helpers.md),
[`lossy()`](https://lj-jenkins.github.io/favr/reference/favr_casting_recycling_helpers.md),
[`recycle()`](https://lj-jenkins.github.io/favr/reference/favr_casting_recycling_helpers.md)
and
[`coerce()`](https://lj-jenkins.github.io/favr/reference/favr_casting_recycling_helpers.md)).

### Soft-Deprecated

- [`abort_if_not()`](https://lj-jenkins.github.io/favr/reference/abort_if_not.md)
  should be replaced with
  [`abortifnot()`](https://lj-jenkins.github.io/favr/reference/abortifnot.md)
  or [`check()`](https://lj-jenkins.github.io/favr/reference/check.md).
- [`cast_if_not()`](https://lj-jenkins.github.io/favr/reference/cast_if_not.md)
  and
  [`recycle_if_not()`](https://lj-jenkins.github.io/favr/reference/recycle_if_not.md)
  should be replaced with their `vctrs` equivalents,
  [`vctrs::vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.html)
  and
  [`vctrs::vec_recycle()`](https://vctrs.r-lib.org/reference/vec_recycle.html),
  respectively.
- All `are_*()` functions and the
  [`have_names()`](https://lj-jenkins.github.io/favr/reference/are_named.md)
  function - their use cases were limited and are easily replicated
  using [`base::vapply()`](https://rdrr.io/r/base/lapply.html) and the
  associated predicate function.

### Bug Fixes

- Fixed a bug where
  [`are_scalar_integerish()`](https://lj-jenkins.github.io/favr/reference/are_integerish.md)
  would immediately error due to an erroneous `.n` argument.

## favr 1.0.0

CRAN release: 2025-12-15

- Added a `NEWS.md` file to track changes to the package.
