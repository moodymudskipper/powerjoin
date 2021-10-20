#' Build a checklist for power joins
#'
#' @param implicit_by WIP
#' @param column_conflict WIP
#' @param duplicate_keys_left WIP
#' @param duplicate_keys_right WIP
#' @param unmatched_keys_left WIP
#' @param unmatched_keys_right WIP
#' @param missing_key_combination_left WIP
#' @param missing_key_combination_right WIP
#' @param inconsistent_factor_levels WIP
#' @param inconsistent_type WIP
#'
#' @export
pj_check <- function(
  implicit_by = c("inform", NA, "warn", "abort"), #
  column_conflict = c(NA, "inform", "warn", "abort"),
  duplicate_keys_left = c(NA, "inform", "warn", "abort"),
  duplicate_keys_right = c(NA, "inform", "warn", "abort"),
  unmatched_keys_left = c(NA, "inform", "warn", "abort"),
  unmatched_keys_right = c(NA, "inform", "warn", "abort"),
  missing_key_combination_left = c(NA, "inform", "warn", "abort"),
  missing_key_combination_right = c(NA, "inform", "warn", "abort"),
  inconsistent_factor_levels = c(NA, "inform", "warn", "abort"),
  inconsistent_type = c(NA, "inform", "warn", "abort")
) {
  implicit_by <- match.arg(as.character(implicit_by), implicit_by)
  column_conflict <- match.arg(as.character(column_conflict), column_conflict)
  duplicate_keys_left <- match.arg(as.character(duplicate_keys_left), duplicate_keys_left)
  duplicate_keys_right <- match.arg(as.character(duplicate_keys_right), duplicate_keys_right)
  unmatched_keys_left <- match.arg(as.character(unmatched_keys_left), unmatched_keys_left)
  unmatched_keys_right <- match.arg(as.character(unmatched_keys_right), unmatched_keys_right)
  missing_key_combination_left <- match.arg(as.character(missing_key_combination_left), missing_key_combination_left)
  missing_key_combination_right <- match.arg(as.character(missing_key_combination_right), missing_key_combination_right)
  inconsistent_factor_levels <- match.arg(as.character(inconsistent_factor_levels), inconsistent_factor_levels)
  inconsistent_type <- match.arg(as.character(inconsistent_type), inconsistent_type)
  dplyr::lst(
    implicit_by, column_conflict,
    duplicate_keys_left, duplicate_keys_right,
    unmatched_keys_left, unmatched_keys_right,
    missing_key_combination_left, missing_key_combination_right,
    inconsistent_factor_levels, inconsistent_type)
}





