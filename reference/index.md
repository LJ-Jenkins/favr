# Package index

## General validation

Tools for general validation depending on input.

- [`abortifnot()`](https://lj-jenkins.github.io/favr/reference/abortifnot.md)
  [`abortif()`](https://lj-jenkins.github.io/favr/reference/abortifnot.md)
  : Ensure the truth of R expressions
- [`check()`](https://lj-jenkins.github.io/favr/reference/check.md)
  [`check_with()`](https://lj-jenkins.github.io/favr/reference/check.md)
  : Check the truth of tidy evaluated expressions
- [`walk_check()`](https://lj-jenkins.github.io/favr/reference/walk-check.md)
  : Apply a predicate check to each element of a vector
- [`check_inherits()`](https://lj-jenkins.github.io/favr/reference/inheritance-checks.md)
  [`check_class()`](https://lj-jenkins.github.io/favr/reference/inheritance-checks.md)
  : Check class inheritance of an object

## Specific validation

Strongly typed validation.

### Types

- [`check_list()`](https://lj-jenkins.github.io/favr/reference/type-checks.md)
  [`check_atomic()`](https://lj-jenkins.github.io/favr/reference/type-checks.md)
  [`check_vector()`](https://lj-jenkins.github.io/favr/reference/type-checks.md)
  [`check_integer()`](https://lj-jenkins.github.io/favr/reference/type-checks.md)
  [`check_double()`](https://lj-jenkins.github.io/favr/reference/type-checks.md)
  [`check_complex()`](https://lj-jenkins.github.io/favr/reference/type-checks.md)
  [`check_character()`](https://lj-jenkins.github.io/favr/reference/type-checks.md)
  [`check_logical()`](https://lj-jenkins.github.io/favr/reference/type-checks.md)
  [`check_raw()`](https://lj-jenkins.github.io/favr/reference/type-checks.md)
  [`check_bytes()`](https://lj-jenkins.github.io/favr/reference/type-checks.md)
  [`check_null()`](https://lj-jenkins.github.io/favr/reference/type-checks.md)
  [`check_numeric()`](https://lj-jenkins.github.io/favr/reference/type-checks.md)
  : Type checks
- [`check_array()`](https://lj-jenkins.github.io/favr/reference/array-type-checks.md)
  [`check_matrix()`](https://lj-jenkins.github.io/favr/reference/array-type-checks.md)
  [`check_table()`](https://lj-jenkins.github.io/favr/reference/array-type-checks.md)
  : Array type checks
- [`check_date()`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md)
  [`check_posixct()`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md)
  [`check_posixlt()`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md)
  [`check_factor()`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md)
  [`check_ordered()`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md)
  [`check_vctr()`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md)
  [`check_list_of()`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md)
  [`check_data_frame()`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md)
  [`check_tibble()`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md)
  [`check_data_table()`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md)
  [`check_tidytable()`](https://lj-jenkins.github.io/favr/reference/s3-type-checks.md)
  : S3 type checks
- [`check_scalar_list()`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md)
  [`check_scalar_atomic()`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md)
  [`check_scalar_vector()`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md)
  [`check_scalar_integer()`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md)
  [`check_scalar_double()`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md)
  [`check_scalar_complex()`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md)
  [`check_scalar_character()`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md)
  [`check_scalar_logical()`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md)
  [`check_scalar_raw()`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md)
  [`check_scalar_bytes()`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md)
  [`check_scalar_numeric()`](https://lj-jenkins.github.io/favr/reference/scalar-type-checks.md)
  : Scalar type checks

### Values

- [`check_true()`](https://lj-jenkins.github.io/favr/reference/scalar-value-checks.md)
  [`check_false()`](https://lj-jenkins.github.io/favr/reference/scalar-value-checks.md)
  [`check_bool()`](https://lj-jenkins.github.io/favr/reference/scalar-value-checks.md)
  [`check_string()`](https://lj-jenkins.github.io/favr/reference/scalar-value-checks.md)
  : Scalar value checks
- [`check_no_na()`](https://lj-jenkins.github.io/favr/reference/forbidden-value-checks.md)
  [`check_finite()`](https://lj-jenkins.github.io/favr/reference/forbidden-value-checks.md)
  [`check_nzchar()`](https://lj-jenkins.github.io/favr/reference/forbidden-value-checks.md)
  : Forbidden value checks

### Modifiers

- [`bare()`](https://lj-jenkins.github.io/favr/reference/modifiers.md)
  [`at_least()`](https://lj-jenkins.github.io/favr/reference/modifiers.md)
  [`at_most()`](https://lj-jenkins.github.io/favr/reference/modifiers.md)
  [`in_range()`](https://lj-jenkins.github.io/favr/reference/modifiers.md)
  : Modify the behaviour of type checking functions

### File system

- [`check_dir()`](https://lj-jenkins.github.io/favr/reference/path-checks.md)
  [`check_file()`](https://lj-jenkins.github.io/favr/reference/path-checks.md)
  [`check_ext()`](https://lj-jenkins.github.io/favr/reference/path-checks.md)
  : File and directory existence checks

## Check builders

Tools to build checks in the style of favr.

- [`s3_vec_check()`](https://lj-jenkins.github.io/favr/reference/s3-check-builders.md)
  [`s3_df_check()`](https://lj-jenkins.github.io/favr/reference/s3-check-builders.md)
  : S3 check builders
