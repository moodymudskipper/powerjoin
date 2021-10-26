#' @import dplyr
#' @importFrom tidyselect peek_vars matches
#' @importFrom utils capture.output getFromNamespace
#' @importFrom methods allNames
#' @importFrom stats setNames
#' @importFrom glue glue glue_collapse
#' @importFrom tidyr nest pivot_longer pivot_wider
#' @importFrom purrr map map_lgl map_chr
#' @importFrom vctrs vec_cast vec_ptype_common vec_slice vec_assign vec_equal_na
#' @importFrom vctrs vec_group_loc vec_in vec_match vec_ptype_full vec_size
#' @import rlang
#' @keywords internal
"_PACKAGE"

globalVariables(c(
  "type",
  "data",
  "name",
  "value"
))

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
