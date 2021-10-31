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
  implicit_keys = c("inform", "ignore", "warn", "abort"), #
  column_conflict = c("ignore", "inform", "warn", "abort"),
  duplicate_keys_left = c("ignore", "inform", "warn", "abort"),
  duplicate_keys_right = c("ignore", "inform", "warn", "abort"),
  unmatched_keys_left = c("ignore", "inform", "warn", "abort"),
  unmatched_keys_right = c("ignore", "inform", "warn", "abort"),
  missing_key_combination_left = c("ignore", "inform", "warn", "abort"),
  missing_key_combination_right = c("ignore", "inform", "warn", "abort"),
  inconsistent_factor_levels = c("ignore", "inform", "warn", "abort"),
  inconsistent_type = c("ignore", "inform", "warn", "abort")
) {
  implicit_keys <-
    if(missing(implicit_keys)) NULL else match.arg(implicit_keys)
  column_conflict <-
    if(missing(column_conflict)) NULL else match.arg(column_conflict)
  duplicate_keys_left <-
    if(missing(duplicate_keys_left)) NULL else match.arg(duplicate_keys_left)
  duplicate_keys_right <-
    if(missing(duplicate_keys_right)) NULL else match.arg(duplicate_keys_right)
  unmatched_keys_left <-
    if(missing(unmatched_keys_left)) NULL else match.arg(unmatched_keys_left)
  unmatched_keys_right <-
    if(missing(unmatched_keys_right)) NULL else match.arg(unmatched_keys_right)
  missing_key_combination_left <-
    if(missing(missing_key_combination_left)) NULL else match.arg(missing_key_combination_left)
  missing_key_combination_right <-
    if(missing(missing_key_combination_right)) NULL else match.arg(missing_key_combination_right)
  inconsistent_factor_levels <-
    if(missing(inconsistent_factor_levels)) NULL else match.arg(inconsistent_factor_levels)
  inconsistent_type <-
    if(missing(inconsistent_type)) NULL else match.arg(inconsistent_type)

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
  if(is.null(res)) res <- character()
  class(res) <- "powerjoin_check"
  res
}


#' Inform on all potential issues
#'
#' This is the output of `check_specs()` with all arguments set to `"inform"`,
#'   it's useful for a complete join diagnostic.
#' @export
full_diagnostic <- check_specs(
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

complete_specs <- function(x) {
  default <- c(
    implicit_keys = "inform",
    column_conflict = "ignore",
    duplicate_keys_left = "ignore",
    duplicate_keys_right = "ignore",
    unmatched_keys_left = "ignore",
    unmatched_keys_right = "ignore",
    missing_key_combination_left = "ignore",
    missing_key_combination_right = "ignore",
    inconsistent_factor_levels = "ignore",
    inconsistent_type = "ignore")
  missing_nms <- setdiff(names(default), names(x))
  # add missing elements
  x[missing_nms] <- default[missing_nms]
  # sort back
  x[names(default)]
}

#' @export
print.powerjoin_check <- function(x, ...) {
  x_complete <- complete_specs(x)
  missing_nms_lgl <- !names(x_complete) %in% names(x)
  mapper <- c(ignore = ">", inform = "i", warn = "!", abort = "x")
  icons <- mapper[x_complete]
  conflicts <- names(x_complete)
  conflicts[icons == ">"] <- cli::col_grey(conflicts[icons == ">"])
  conflicts[icons == "i"] <- cli::col_blue(conflicts[icons == "i"])
  conflicts[icons == "x"] <- cli::col_red(conflicts[icons == "x"])
  conflicts[icons == "!"] <- cli::col_yellow(conflicts[icons == "!"])
  conflicts[missing_nms_lgl] <- cli::style_italic(conflicts[missing_nms_lgl])
  conflicts[!missing_nms_lgl] <- cli::style_bold(conflicts[!missing_nms_lgl])
  writeLines(cli::col_grey("# powerjoin check specifications"))
  writeLines(rlang::format_error_bullets(setNames(conflicts, icons)))
  invisible(x)
}

#' @export
c.powerjoin_check <- function(...) {
  default_nms <- c("implicit_keys", "column_conflict", "duplicate_keys_left",
                   "duplicate_keys_right", "unmatched_keys_left", "unmatched_keys_right",
                   "missing_key_combination_left", "missing_key_combination_right",
                   "inconsistent_factor_levels", "inconsistent_type")

  res <- unlist(rev(list(...)))
  res <- res[intersect(default_nms, names(res))]
  class(res) <- "powerjoin_check"
  res
}

check_duplicate_keys_left <- function(x, by, check) {
  if(check[["duplicate_keys_left"]] != "ignore" &&
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
  if(check[["duplicate_keys_right"]] != "ignore" &&
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
  if(check[["missing_key_combination_left"]] != "ignore" &&
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
  if(check[["missing_key_combination_right"]] != "ignore" &&
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
  if(check[["inconsistent_factor_levels"]] != "ignore") {
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
if(check[["inconsistent_type"]] != "ignore") {
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
  if(check[["unmatched_keys_left"]] != "ignore" && length(rows$x_unmatched)) {
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
  if(check[["unmatched_keys_right"]] != "ignore" && length(rows$y_unmatched)) {
    fun <- getFromNamespace(check[["unmatched_keys_right"]], "rlang")
    unmatched_combos <- distinct(y_in[rows$y_unmatched, by])
    msg <- paste0(
      "Keys in the right table have unmatched combinations:\n",
      paste(capture.output(unmatched_combos), collapse = "\n")
    )
    fun(msg)
  }
}
