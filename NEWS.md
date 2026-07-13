# favr (development version)

abortifnot() abortif(), deprecated abort_if_not() abort_if() 

check_type() & is_type() and check_class() & is_class()

Type checks

Scalar type checks

Scalar value checks


* New `check()` function to check an object with a given predicate and error if the predicate returns `FALSE`.

* New `is_type()`, `is_class()`, `check_type()`, and `check_class()` functions to check the type/class of an object against another object, with multiple matching options.

* Removed the online documentation for `enforce`, `schema`, and the associated casting and recycling helpers, as I am questioning their place in the new direction of the package.

* Deprecated softly all `are_*` functions, as well as `abort_if_not`, `cast_if_not`, and `recycle_if_not`.

* Fixed a bug where `are_scalar_integerish()` would immediately error due to an erroneous `.n` argument.

# favr 1.0.0

* Added a `NEWS.md` file to track changes to the package.
