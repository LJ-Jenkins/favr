bare_atomic_types <- function(n = 1L) {
  list(
    logical(n),
    integer(n),
    double(n),
    numeric(n),
    complex(n),
    character(n),
    raw(n)
  )
}

classed_atomic_types <- function(n = 1L) {
  list(
    factor(letters[1:n]),
    ordered(letters[1:n]),
    as.Date(Sys.Date() + seq_len(n)),
    as.POSIXct(Sys.time() + seq_len(n))
  )
}

bare_vector_types <- function(n = 1L) {
  c(
    list(vector("list", length = n)),
    bare_atomic_types(n),
    list(matrix(seq_len(n))) # matrix is bare as the class vector is not in `attributes(x)`
  )
}

classed_vector_types <- function(n = 1L) {
  c(
    classed_atomic_types(n),
    list(
      # behaviour unusual as POSIXlt is a list, so `is_list()`, `is_vector()`
      # return `TRUE` but the `n` argument is always `11` as there are `11`
      # list elements to the `POSIXlt` class for the different datetime
      # elements.
      # `length()` returns the length of the list elements whereas `n`
      # always checks the length of the top level list.

      # as.POSIXlt(Sys.time() + seq_len(n)),
      data.frame(x = seq_len(n))
    )
  )
}

na_atomics <- function() {
  list(
    NA,
    NA_integer_,
    NA_real_,
    NA_complex_,
    NA_character_
  )
}
