#' Build a checklist for power joins
#'
#' @param implicit_keys WIP
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
check_specs <- function(
  implicit_keys = c("inform", NA, "warn", "abort"), #
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
  implicit_keys <- match.arg(as.character(implicit_keys), implicit_keys)
  column_conflict <- match.arg(as.character(column_conflict), column_conflict)
  duplicate_keys_left <- match.arg(as.character(duplicate_keys_left), duplicate_keys_left)
  duplicate_keys_right <- match.arg(as.character(duplicate_keys_right), duplicate_keys_right)
  unmatched_keys_left <- match.arg(as.character(unmatched_keys_left), unmatched_keys_left)
  unmatched_keys_right <- match.arg(as.character(unmatched_keys_right), unmatched_keys_right)
  missing_key_combination_left <- match.arg(as.character(missing_key_combination_left), missing_key_combination_left)
  missing_key_combination_right <- match.arg(as.character(missing_key_combination_right), missing_key_combination_right)
  inconsistent_factor_levels <- match.arg(as.character(inconsistent_factor_levels), inconsistent_factor_levels)
  inconsistent_type <- match.arg(as.character(inconsistent_type), inconsistent_type)
  res <- c(
    implicit_keys = implicit_keys,
    column_conflict = column_conflict,
    duplicate_keys_left = duplicate_keys_left,
    duplicate_keys_right = duplicate_keys_right,
    unmatched_keys_left = unmatched_keys_left,
    unmatched_keys_right = unmatched_keys_right,
    missing_key_combination_left = missing_key_combination_left,
    missing_key_combination_right = missing_key_combination_right,
    inconsistent_factor_levels = inconsistent_factor_levels,
    inconsistent_type = inconsistent_type)
  class(res) <- "powerjoin_check"
  res
}

#' @export
always_inform <- check_specs(
  implicit_keys = "inform",
  column_conflict = "inform",
  duplicate_keys_left = "inform",
  duplicate_keys_right = "inform",
  unmatched_keys_left =  "inform",
  unmatched_keys_right = "inform",
  missing_key_combination_left = "inform",
  missing_key_combination_right = "inform",
  inconsistent_factor_levels = "inform",
  inconsistent_type = "inform")

#' @export
print.powerjoin_check <- function(x, ...) {
  mapper <- c(inform = "i", warn = "!", abort = "x")
  icons <- mapper[x]
  icons[is.na(icons)] <- ">"
  conflicts <- names(x)
  conflicts[icons == ">"] <- cli::col_grey(cli::style_italic(conflicts[icons == ">"]))
  conflicts[icons == "i"] <- cli::col_blue(conflicts[icons == "i"])
  conflicts[icons == "x"] <- cli::col_red(conflicts[icons == "x"])
  conflicts[icons == "!"] <- cli::col_yellow(conflicts[icons == "!"])
  writeLines(cli::col_grey("# powerjoin check specifications"))
  writeLines(rlang::format_error_bullets(setNames(conflicts, icons)))
  invisible(x)
}

#' @export
c.powerjoin_check <- function(...) {
  dplyr::coalesce(!!!rev(list(...)))
}



check_duplicate_keys_left <- function(x, by, check) {
  if(!is.na(check[["duplicate_keys_left"]]) &&
     n_distinct(x[by]) != nrow(x)) {
    fun <- getFromNamespace(check[["duplicate_keys_left"]], "rlang")
    dupes <- x[by]
    dupes <- dupes[duplicated(dupes),]
    msg <- paste0(
      "Keys in the left table have duplicates:\n",
      paste(capture.output(dupes), collapse = "\n")
    )
    fun(msg)
  }
}

check_duplicate_keys_right <- function(y, by, check) {
  if(!is.na(check[["duplicate_keys_right"]]) &&
     n_distinct(y[by]) != nrow(y)) {
    fun <- getFromNamespace(check[["duplicate_keys_right"]], "rlang")
    dupes <- y[by]
    dupes <- dupes[duplicated(dupes),]
    msg <- paste0(
      "Keys in the right table have duplicates:\n",
      paste(capture.output(dupes), collapse = "\n")
    )
    fun(msg)
  }
}

check_missing_key_combination_left <- function(x, by, check) {
  if(!is.na(check[["missing_key_combination_left"]]) &&
     n_distinct(x[by]) != prod(vapply(x[by], n_distinct, numeric(1)))) {
    fun <- getFromNamespace(check[["missing_key_combination_left"]], "rlang")
    all_combos <- exec(
      "expand.grid",
      !!! lapply(x[by], unique),
      stringsAsFactors = FALSE)
    missing_combos <- setdiff(all_combos, x[by])
    missing_combos <- as_tibble(missing_combos)
    msg <- paste0(
      "Keys in the left table have missing combinations:\n",
      paste(capture.output(missing_combos), collapse = "\n")
    )
    fun(msg)
  }
}


check_missing_key_combination_right <- function(y, by, check) {
  if(!is.na(check[["missing_key_combination_right"]]) &&
     n_distinct(y[by]) != prod(vapply(y[by], n_distinct, numeric(1)))) {
    fun <- getFromNamespace(check[["missing_key_combination_right"]], "rlang")
    all_combos <- exec(
      "expand.grid",
      !!! lapply(y[by], unique),
      stringsAsFactors = FALSE)
    missing_combos <- setdiff(all_combos, y[by])
    missing_combos <- as_tibble(missing_combos)
    msg <- paste0(
      "Keys in the right table have missing combinations:\n",
      paste(capture.output(missing_combos), collapse = "\n")
    )
    fun(msg)
  }
}

check_inconsistent_factor_levels <- function(x_in, y_in, by, check) {
  if(!is.na(check[["inconsistent_factor_levels"]])) {
    fun <- getFromNamespace(check[["inconsistent_factor_levels"]], "rlang")
    if(check[["inconsistent_factor_levels"]] == "abort") {
      msg <- mapply(function(x, y, x_nm, y_nm) {
        if(is.factor(x) && is.factor(y) && !setequal(levels(x), levels(y))) {
          if(x_nm == y_nm)
            sprintf("`%s` has different factor levels in the left and right tables", x_nm)
          else
            sprintf("`%s` and `%s` (in resp. left and right table) have different factor levels", x_nm, y_nm)
        }}, x_in[by$x], y_in[by$y], names(x_in[by$x]), names(y_in[by$y]))
    } else {
      msg <- mapply(function(x, y, x_nm, y_nm) {
        if(is.factor(x) && is.factor(y) && !setequal(levels(x), levels(y))) {
          if(x_nm == y_nm)
            sprintf("`%s` has different factor levels in the left and right tables, coercing to character vector", x_nm)
          else
            sprintf("`%s` and `%s` (in resp. the left and right table) have different factor levels, coercing to character vector", x_nm, y_nm)
        }}, x_in[by$x], y_in[by$y], names(x_in[by$x]), names(y_in[by$y]))
    }
    msg <- msg[lengths(msg)>0]
    msg <- paste(msg, collapse = "\n")
    fun(msg)
  }
}

check_inconsistent_type <- function(x_in, y_in, by, check) {
if(!is.na(check[["inconsistent_type"]])) {
  fun <- getFromNamespace(check[["inconsistent_type"]], "rlang")
  msg <- mapply(function(x, y, x_nm, y_nm) {
    if(!identical(typeof(x), typeof(y)) || !identical(class(x), class(y))) {
      if(x_nm == y_nm)
        sprintf("`%s` has a different type or class in the left and right tables", x_nm)
      else
        sprintf("`%s` and `%s` (in resp. left and right table) have different types or classes", x_nm, y_nm)
    }}, x_in[by$x], y_in[by$y], names(x_in[by$x]), names(y_in[by$y]))
  msg <- msg[lengths(msg)>0]
  if(length(msg)) {
    msg <- paste(msg, collapse = "\n")
    fun(msg)
  }
}
}

check_unmatched_keys_left <- function(x_in, y_in, by, rows, check) {
  if(!is.na(check[["unmatched_keys_left"]]) && length(rows$x_unmatched)) {
    fun <- getFromNamespace(check[["unmatched_keys_left"]], "rlang")
    unmatched_combos <- distinct(x_in[rows$x_unmatched, by])
    msg <- paste0(
      "Keys in the left table have unmatched combinations:\n",
      paste(capture.output(unmatched_combos), collapse = "\n")
    )
    fun(msg)
  }
}

check_unmatched_keys_right <- function(x_in, y_in, by, rows, check) {
  if(!is.na(check[["unmatched_keys_right"]]) && length(rows$y_unmatched)) {
    fun <- getFromNamespace(check[["unmatched_keys_right"]], "rlang")
    unmatched_combos <- distinct(y_in[rows$y_unmatched, by])
    msg <- paste0(
      "Keys in the right table have unmatched combinations:\n",
      paste(capture.output(unmatched_combos), collapse = "\n")
    )
    fun(msg)
  }
}
