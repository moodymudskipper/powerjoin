#' @importFrom dplyr auto_copy as_tibble group_vars ungroup mutate group_by_at
#' @importFrom dplyr select_at vars bind_cols arrange full_join dplyr_reconstruct
#' @importFrom dplyr select transmute coalesce summarize tibble left_join n_distinct
#' @importFrom dplyr distinct rename one_of %>% across
#' @importFrom dplyr setdiff union intersect setequal
#' @importFrom tidyselect peek_vars matches
#' @importFrom utils capture.output getFromNamespace
#' @importFrom methods allNames
#' @importFrom stats setNames
#' @importFrom glue glue glue_collapse
#' @importFrom tidyr nest pivot_longer pivot_wider
#' @importFrom purrr map map_lgl map_chr
#' @importFrom vctrs vec_cast vec_ptype_common vec_slice vec_assign vec_detect_missing
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
