# check_ext(), check_dir() and check_file() errors on non-string input

    Code
      check_ext(1, ext = ".csv")
    Condition
      Error:
      ! `1` must be a single string, not the number 1.
    Code
      check_ext(c("w", "e"), ext = ".csv")
    Condition
      Error:
      ! `c("w", "e")` must be a single string, not a <character> vector of length 2.

---

    Code
      check_dir(1)
    Condition
      Error:
      ! `1` must be a single string, not the number 1.
    Code
      check_dir(c("w", "e"))
    Condition
      Error:
      ! `c("w", "e")` must be a single string, not a <character> vector of length 2.

---

    Code
      check_file(1)
    Condition
      Error:
      ! `1` must be a single string, not the number 1.
    Code
      check_file(c("w", "e"))
    Condition
      Error:
      ! `c("w", "e")` must be a single string, not a <character> vector of length 2.

# check_dir() and check_file() don't duplicate path if typed

    Code
      check_dir("non_existing_dir")
    Condition
      Error:
      ! `x` must be an existing directory, but it doesn't exist.
      i Path provided: 'non_existing_dir'.
    Code
      check_file("non_existing_file")
    Condition
      Error:
      ! `x` must be an existing file, but it doesn't exist.
      i Path provided: 'non_existing_file'.
    Code
      a_var <- "non_existing_dir"
      check_dir(a_var)
    Condition
      Error:
      ! `a_var` must be an existing directory, but it doesn't exist.
      i Path provided: 'non_existing_dir'.
    Code
      check_file(a_var)
    Condition
      Error:
      ! `a_var` must be an existing file, but it doesn't exist.
      i Path provided: 'non_existing_dir'.

# check_ext() and check_file() errors on wrong extension

    Code
      check_ext(f, ext = ".csv")
    Condition
      Error:
      ! `f` must have extension ".csv".
    Code
      check_file(f, ext = ".csv")
    Condition
      Error:
      ! `f` must have extension ".csv".

---

    Code
      check_ext(f, ext = c(".csv", ".xlsx"))
    Condition
      Error:
      ! `f` must have extension ".csv" or ".xlsx".
    Code
      check_file(f, ext = c(".csv", ".xlsx"))
    Condition
      Error:
      ! `f` must have extension ".csv" or ".xlsx".

# check_ext() and check_file() errors on wrong extension with case sensitivity

    Code
      check_ext(f, ext = ".CSV")
    Condition
      Error:
      ! `f` must have extension ".CSV".
    Code
      check_file(f, ext = ".CSV")
    Condition
      Error:
      ! `f` must have extension ".CSV".

---

    Code
      check_ext(f, ext = c(".CSV", ".XLSX"))
    Condition
      Error:
      ! `f` must have extension ".CSV" or ".XLSX".
    Code
      check_file(f, ext = c(".CSV", ".XLSX"))
    Condition
      Error:
      ! `f` must have extension ".CSV" or ".XLSX".

# check_ext() errors on non-chr or empty string ext input

    Code
      check_ext("a", ext = 1)
    Condition
      Error:
      ! `1` must be a <character> vector, not the number 1.
    Code
      check_ext("a", ext = c(".csv", ""))
    Condition
      Error:
      ! `c(".csv", "")` must not contain empty strings.
    Code
      check_ext("a", ext = c(".csv", NA))
    Condition
      Error in `if (any(endsWith(x, ext))) ...`:
      ! missing value where TRUE/FALSE needed
    Code
      check_ext("a", ext = character(0))
    Condition
      Error:
      ! `character(0)` must be a <character> vector of at least length 1, but it is of length 0.

