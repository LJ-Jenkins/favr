test_that("abort_if* no error for TRUE/FALSE", {
  expect_no_error(abort_if_not(TRUE))
  expect_no_error(abort_if_not(c(TRUE, TRUE)))
  x <- 1:10
  expect_no_error(abort_if_not(is.integer(x)))
  expect_no_error(abort_if_not(is.integer(x), is.numeric(x)))
  expect_no_error(abort_if_not(quote(TRUE)))

  expect_no_error(abort_if(FALSE))
  expect_no_error(abort_if(c(FALSE, FALSE)))
  expect_no_error(abort_if(is.double(x)))
  expect_no_error(abort_if(is.character(x), is.list(x)))
  expect_no_error(abort_if(quote(FALSE)))
})

test_that("abort_if* error for FALSE/TRUE", {
  expect_error(abort_if_not(FALSE))
  expect_error(abort_if_not(c(TRUE, FALSE)))
  x <- 1:10
  expect_error(abort_if_not(!is.integer(x)))
  expect_error(abort_if_not(is.integer(x), !is.numeric(x)))
  expect_error(abort_if_not(!is.integer(x), is.numeric(x)))
  expect_error(abort_if_not(quote(FALSE)))

  expect_error(abort_if(TRUE))
  expect_error(abort_if(c(TRUE, FALSE)))
  x <- 1:10
  expect_error(abort_if(is.integer(x)))
  expect_error(abort_if(is.integer(x), !is.numeric(x)))
  expect_error(abort_if(!is.integer(x), is.numeric(x)))
  expect_error(abort_if(quote(TRUE)))
})

test_that("abort_if* error for NA", {
  expect_error(abort_if_not(NA))
  expect_error(abort_if_not(c(TRUE, NA)))
  x <- c(1, 2, NA, 10)
  expect_error(abort_if_not(x > 0))
  expect_error(abort_if(NA))
  expect_error(abort_if(c(FALSE, NA)))
  expect_error(abort_if(x > 0))
})

test_that("abort_if* error when arg isn't logical", {
  expect_error(abort_if_not(10))
  expect_error(abort_if_not(NULL))
  expect_error(abort_if_not(list(TRUE)))
  expect_error(abort_if_not(data.frame(x = TRUE)))
  expect_error(abort_if_not(\(x) TRUE))
  expect_error(abort_if(10))
  expect_error(abort_if(NULL))
  expect_error(abort_if(list(TRUE)))
  expect_error(abort_if(data.frame(x = TRUE)))
  expect_error(abort_if(\(x) TRUE))
})

test_that("abort_if* works with injection", {
  a_var <- TRUE
  a_msg <- "var_a"
  a_var_name <- "a"
  a_var_list <- list(a = TRUE, b = TRUE)
  a_var_list2 <- list(a = TRUE, b = TRUE, c = FALSE)
  glue_list <- list(a = TRUE, "a_glue_msg_for_{a_msg}" = FALSE)
  expect_no_error(abort_if_not(!!a_var))
  expect_no_error(abort_if_not(!!!a_var_list))
  expect_error(abort_if_not(!!!a_var_list2))
  expect_no_error(abort_if_not(!!a_var_name := !!a_var))
  expect_no_error(abort_if_not({{ a_var }}))
  expect_no_error(abort_if_not({{ a_var_name }} := {{ a_var }}))
  expect_no_error(abort_if_not({{ a_var_name }} := !!a_var))
  expect_error(
    abort_if_not("a_glue_msg_for_{a_msg}" = FALSE),
    regexp = "a_glue_msg_for_var_a"
  )
  expect_error(
    abort_if_not(!!!glue_list),
    regexp = "a_glue_msg_for_var_a"
  )
  a_var_list <- list(a = FALSE, b = FALSE)
  a_var_list2 <- list(a = FALSE, b = FALSE, c = TRUE)
  glue_list <- list(a = FALSE, "a_glue_msg_for_{a_msg}" = TRUE)
  expect_error(abort_if({{ a_var_name }} := !!a_var))
  expect_no_error(abort_if(!!!a_var_list))
  expect_error(abort_if(!!!a_var_list2, regexp = "c"))
  expect_error(
    abort_if(!!!glue_list),
    regexp = "a_glue_msg_for_var_a"
  )
})
