#' @import cli
#' @import rlang
#' @import vctrs
#' @import tidyselect
NULL

on_load(
  local_use_cli(
    format = TRUE,
    inline = TRUE
  )
)
