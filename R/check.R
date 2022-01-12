#' Build a checklist for power joins
#'
#' @param implicit_keys What to do if keys are not given explicitly through the
#'   `by` argument
#' @param column_conflict What to do if the join creates a column conflict which
#'   is not handled by the `conflict` argument
#' @param duplicate_keys_left What to do if we find duplicate sets of keys in the
#'   left table
#' @param duplicate_keys_right What to do if we find duplicate sets of keys in the
#'   right table
#' @param unmatched_keys_left What to do if we find unmatched sets of keys in the
#'   left table
#' @param unmatched_keys_right What to do if we find unmatched sets of keys in the
#'   right table
#' @param missing_key_combination_left What to do if the left table doesn't contain
#'   all key combinations
#' @param missing_key_combination_right What to do if the right table doesn't contain
#'   all key combinations
#' @param inconsistent_factor_levels What to do if the key columns from both sides
#'   have inconsistent factor levels
#' @param inconsistent_type What to do if we joined keys have a different type
#' @param grouped_input What to do if one or both of the tables are grouped
#' @param na_keys What to do if keys contain missing values
#' @return A character vector of class `"powerjoin_check"`
#'
#' @export
#' @examples
#' check_specs(
#'   implicit_keys = "ignore",
#'   grouped_input = "inform",
#'   column_conflict = "abort",
#'   na_keys ="warn")
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
  inconsistent_type = c("ignore", "inform", "warn", "abort"),
  grouped_input = c("ignore", "inform", "warn", "abort"),
  na_keys = c("ignore", "inform", "warn", "abort")
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
  grouped_input <-
    if(missing(grouped_input)) NULL else match.arg(grouped_input)
  na_keys <-
    if(missing(na_keys)) NULL else match.arg(na_keys)

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
    inconsistent_type = inconsistent_type,
    grouped_input = grouped_input,
    na_keys = na_keys)
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
  inconsistent_type = "inform",
  grouped_input = "inform",
  na_keys = "inform")

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
    inconsistent_type = "ignore",
    grouped_input = "ignore",
    na_keys = "ignore")
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
  cli::cli_bullets(setNames(conflicts, icons))
  invisible(x)
}

#' @export
c.powerjoin_check <- function(...) {
  default_nms <- c("implicit_keys", "column_conflict", "duplicate_keys_left",
                   "duplicate_keys_right", "unmatched_keys_left", "unmatched_keys_right",
                   "missing_key_combination_left", "missing_key_combination_right",
                   "inconsistent_factor_levels", "inconsistent_type", "grouped_input",
                   "na_keys")

  res <- unlist(rev(list(...)))
  res <- res[intersect(default_nms, names(res))]
  class(res) <- "powerjoin_check"
  res
}

check_implicit_keys1 <- function(by, check) {
  if(is.null(by) && check[["implicit_keys"]] %in% "abort") {
    abort("`by`is `NULL`, key columns should be explicit")
  }
}

check_implicit_keys2 <- function(by, check) {
  if(check[["implicit_keys"]] != "ignore") {
    by_quoted <- encodeString(by, quote = "\"")
    if (length(by_quoted) == 1L) {
      by_code <- by_quoted
    } else {
      by_code <- paste0(
        "c(", paste(by_quoted, collapse = ", "),
        ")"
      )
    }
    fun <- getFromNamespace(check[["implicit_keys"]], "rlang")
    fun(paste0("Joining, by = ", by_code))
  }
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
          diff1 <- setdiff(levels(x), levels(y))
          diff2 <- setdiff(levels(y), levels(x))
          msg_suffix <- paste(c(
            if (length(diff2)) sprintf("In the left table we don't find %s",
                                       toString(shQuote(diff2))),
            if (length(diff1)) sprintf("In the right table we don't find %s",
                                       toString(shQuote(diff1)))
          ), collapse = "\n")
          if(x_nm == y_nm)
            sprintf("`%s` has different factor levels in the left and right tables:\n%s",
                    x_nm, msg_suffix)
          else
            sprintf("`%s` and `%s` (in resp. left and right table) have different factor levels:\n%s",
                    x_nm, y_nm, msg_suffix)
        }}, x_in[by$x], y_in[by$y], names(x_in[by$x]), names(y_in[by$y]))
    } else {
      msg <- mapply(function(x, y, x_nm, y_nm) {
        if(is.factor(x) && is.factor(y) && !setequal(levels(x), levels(y))) {
          diff1 <- setdiff(levels(x), levels(y))
          diff2 <- setdiff(levels(y), levels(x))
          msg_infix <- paste(c(
            if (length(diff2)) sprintf("In the left table we don't find %s",
                                       toString(shQuote(diff2))),
            if (length(diff1)) sprintf("In the right table we don't find %s",
                                       toString(shQuote(diff1)))
          ), collapse = "\n")
          if(x_nm == y_nm)
            sprintf("`%s` has different factor levels in the left and right tables:\n%s\nCoercing to character vector",
                    x_nm, msg_infix)
          else
            sprintf("`%s` and `%s` (in resp. the left and right table) have different factor levels:\n%s\nCoercing to character vector",
                    x_nm, y_nm, msg_infix)
        }}, x_in[by$x], y_in[by$y], names(x_in[by$x]), names(y_in[by$y]))
    }
    msg <- msg[lengths(msg)>0]
    if(length(msg)) {
      msg <- paste(msg, collapse = "\n")
      fun(msg)
    }
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

check_column_conflict <- function(conflicted_cols, check) {
  if(check[["column_conflict"]] != "ignore" && length(conflicted_cols$unhandled)) {
    fun <- getFromNamespace(check[["column_conflict"]], "rlang")
    if(check[["column_conflict"]] == "abort") {
      msg <- paste("The following columns are conflicted and their conflicts are not handled: ",
                   toString(paste0("'", conflicted_cols$unhandled, "'")))
    } else {
      msg <- paste(
        "The following columns are conflicted and their conflicts are not",
        "handled, they will be suffixed:",
        toString(paste0("'", conflicted_cols$unhandled, "'")))
    }
    fun(msg)
  }
}

check_column_conflict_extra <- function(x_names, y_names, by, keep, check) {
  if(length(by$extra_cols) && check[["column_conflict"]] != "ignore") {
    conflicted_extra <- switch(
      keep,
      left =,
      default = c(
        intersect(x_names, by$extra_cols),
        intersect(setdiff(y_names, by$y), by$extra_cols)),
      right = c(
        intersect(setdiff(x_names, by$x), by$extra_cols),
        intersect(y_names, by$extra_cols)),
      default_fuzzy =,
      both = c(
        intersect(x_names, by$extra_cols),
        intersect(y_names, by$extra_cols)),
      none = c(
        intersect(setdiff(x_names, by$x), by$extra_cols),
        intersect(setdiff(y_names, by$y), by$extra_cols)))
    if(length(conflicted_extra)) {
      fun <- getFromNamespace(check[["column_conflict"]], "rlang")
      if(check[["column_conflict"]] == "abort") {
        msg <- paste(
          "Conflicting columns were created in the fuzzy join:",
          toString(paste0("'", conflicted_extra, "'")))
      } else {
        msg <- paste(
          "Conflicting columns were created in the fuzzy join,",
          "they will be suffixed:",
          toString(paste0("'", conflicted_extra, "'")))
      }
      fun(msg)
    }
  }
}


check_grouped <- function(x, y, check) {
  if(check[["grouped_input"]] != "ignore") {
    fun <- getFromNamespace(check[["grouped_input"]], "rlang")
    if(check[["grouped_input"]] == "abort") {

      msg1 <- if(inherits(x, "grouped_df")) {
        "The left input table is grouped."
      }
      msg2 <- if(inherits(y, "grouped_df")) {
        "The right input table is grouped."
      }
      skip <- ""
    } else {
      msg1 <- if(inherits(x, "grouped_df")) {
        "The left input table is grouped, the output will preserve the grouping unless columns were renamed or removed."
      }
      msg2 <- if(inherits(y, "grouped_df")) {
        "The right input table is grouped, the output will ignore those groups."
      }
      skip <- NULL
    }
    msgs <- c(msg1, msg2)
    if(length(msgs)) {
      fun(paste(c(skip, msgs), collapse = "\n"))
    }
  }
}

check_na_keys <- function(x, y, by, check) {
  if(check[["na_keys"]] != "ignore") {
    fun <- getFromNamespace(check[["na_keys"]], "rlang")
    msg1 <- if(anyNA(x[by$x])) {
      "The left input table has missing keys"
    }
    msg2 <- if(anyNA(y[by$y])) {
      "The right input table has missing keys"
    }
    msgs <- c(msg1, msg2)
    if(length(msgs)) {
      fun(paste(msgs, collapse = "\n"))
    }
  }
}
