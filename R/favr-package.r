#' @import cli
#' @import rlang
#' @import vctrs
#' @import tidyselect
#' @import lifecycle
"_PACKAGE"

on_load(
  local_use_cli(
    format = TRUE,
    inline = TRUE
  )
)
