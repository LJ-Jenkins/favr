# from rlang's standalone-lifecycle.R

local_lifecycle_silence <- function(frame = rlang::caller_env()) {
  rlang::local_options(
    .frame = frame,
    lifecycle_verbosity = "quiet"
  )
}

expect_deprecated <- function(expr, regexp = NULL, ...) {
  rlang::local_options(lifecycle_verbosity = "warning")

  if (!is.null(regexp) && rlang::is_na(regexp)) {
    rlang::abort("`regexp` can't be `NA`.")
  }

  testthat::expect_warning(
    {{ expr }},
    regexp = regexp,
    class = "lifecycle_warning_deprecated",
    ...
  )
}
