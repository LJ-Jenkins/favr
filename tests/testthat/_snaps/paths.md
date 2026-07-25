# check_dir() and check_file() errors on non-string input

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

# check_dir() and check_file() don't duiplicate path if typed

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

# check_dir() informative error if given filepath

    Code
      f <- withr::local_tempfile(lines = "x")
      check_dir(f)
    Condition
      Error:
      ! `f` must be an existing directory, but it doesn't exist.
      i Path provided:
      '<file>'.

# check_file() informative error if given dirpath

    Code
      d <- withr::local_tempdir()
      check_file(d)
    Condition
      Error:
      ! `d` must be an existing file, but it doesn't exist.
      i Path provided:
      '<dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir><dir>'.

