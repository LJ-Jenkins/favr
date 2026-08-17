# check_inherits() exact matching

    Code
      a <- structure(1, class = c("a", "b"))
      check_inherits(a, c("a", "c"), match = "exact")
    Condition
      Error:
      ! `a` must be class <a/c>, but is class <a/b>.

# check_class() exact matching

    Code
      a <- structure(1, class = c("a", "b"))
      check_class(a, c("a", "c"))
    Condition
      Error:
      ! `a` must be class <a/c>, but is class <a/b>.

# check_inherits() any matching

    Code
      a <- structure(1, class = c("a", "b", "c"))
      check_inherits(a, c("d", "e", "f"), match = "any")
    Condition
      Error:
      ! `a` must inherit from any of <d/e/f>, but is class <a/b/c>.

# check_inherits() all matching

    Code
      check_inherits(x, c("b", "z"), match = "all")
    Condition
      Error:
      ! `x` must inherit from all of class <b/z> in order, but is class <a/b/c/d>.

# check_inherits() uses arg in error messages

    Code
      check_inherits(x, "myclass")
    Condition
      Error:
      ! `x` must inherit from <myclass>, but is class <integer>.
    Code
      check_inherits(x, "myclass", arg = "my_x")
    Condition
      Error:
      ! `my_x` must inherit from <myclass>, but is class <integer>.

# check_inherits() forwards args to cli_abort correctly

    Code
      check_inherits(1L, "myclass", footer = "Custom footer")
    Condition
      Error:
      ! `1L` must inherit from <myclass>, but is class <integer>.
      Custom footer

# .envir doesn't interfere with error messages

    Code
      e <- environment()
      e$arg <- "my_arg"
      e$target <- "woops chr class"
      e$x <- "woops chr class"
      check_inherits(1L, "double", .envir = e)
    Condition
      Error:
      ! `1L` must inherit from <double>, but is class <integer>.

---

    Code
      e <- environment()
      e$arg <- "my_arg"
      e$target <- "woops chr class"
      e$x <- "woops chr class"
      check_class(1L, "double", .envir = e)
    Condition
      Error:
      ! `1L` must be class <double>, but is class <integer>.

