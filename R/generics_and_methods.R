
#-------------------------------------------------------------------------------
# left

#' Power joins
#'
#' @param by As in {dplyr}, but extended so user can supply a formula or a list
#'  of character and formulas. Formulas are used for fuzzy joins and
#' @inheritParams dplyr::left_join
#' @param check A list created with `check_specs()`
#' @param conflict A function, formula, the special value amongst `"patch"`,
#'   or a named list of such items.
#' @param fill Values used to replace missing values originating in unmatched keys,
#'   or a named list of such items.
#' @param keep A boolean for compatibility with {dplyr}, or a value among "left",
#' "right", "both", "none" or "default". See details.
#'
#' The vales of the `keep` parameter work as follow :
#'
#' - `NULL` (default) : merge keys and name them as the left table's keys, and
#'   keep columns used for fuzzy joins from both tables
#' - `left` : keep only key columns for left table
#' - `right`: keep only key columns for right table
#' - `both` or `TRUE`: keep key columns from both tables, adding suffix if relevant
#' - `none` : drop all key columns from the output
#' - `FALSE` : merge keys and name them as the left table's keys, maps to `none` for fuzzy joins
#' @return A data frame
#' @export
power_left_join <- function(
  x, y = NULL, by = NULL, copy = FALSE, suffix = c(".x", ".y"), keep = NULL,
  na_matches = c("na", "never"),
  check = check_specs(),
  conflict = NULL,
  fill = NULL) {
  UseMethod("power_left_join")
}

# from dplyr 1.0.7
#' @export
power_left_join.data.frame <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), keep = NULL,
  na_matches = c("na", "never"),
  check = check_specs(),
  conflict = NULL,
  fill = NULL) {

  if(is_bare_list(y)) {
    x <- list(x)
    res <- power_left_join.list(
      x, y, by, copy, suffix, keep, na_matches,
      # powerjoin args
      check, conflict, fill
    )
    return(res)
  }
  y <- auto_copy(x, y, copy = copy)
  join_mutate(x, y,
              by = by, copy = copy, type = "left", suffix = suffix,
              na_matches = na_matches, keep = keep,
              # powerjoin args
              check = check, conflict = conflict, fill = fill
  )
}

#' @export
power_left_join.list <- function(
  x, y = NULL, by = NULL, copy = FALSE, suffix = c(".x", ".y"), keep = NULL,
  na_matches = c("na", "never"),
  check = check_specs(),
  conflict = NULL,
  fill = NULL) {

  if(!is.null(y) &&!is_bare_list(y)) {
    y <- list(y)
  }
  na_matches <- arg_match(na_matches)

  Reduce(function(x,y) power_left_join(
    x, y, by, copy, suffix, keep, na_matches,
    # powerjoin args
    check, conflict, fill
  ), c(x,y))
}


#-------------------------------------------------------------------------------
# right

#' @export
#' @rdname power_left_join
power_right_join <- function(
  x, y = NULL, by = NULL, copy = FALSE, suffix = c(".x", ".y"), keep = NULL,
  na_matches = c("na", "never"),
  check = check_specs(),
  conflict = NULL,
  fill = NULL) {
  UseMethod("power_right_join")
}

# from dplyr 1.0.7
#' @export
power_right_join.data.frame <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), keep = NULL,
  na_matches = c("na", "never"),
  check = check_specs(),
  conflict = NULL,
  fill = NULL) {

  if(is_bare_list(y)) {
    x <- list(x)
    res <- power_right_join.list(
      x, y, by, copy, suffix, keep, na_matches,
      # powerjoin args
      check, conflict, fill
    )
    return(res)
  }
  y <- auto_copy(x, y, copy = copy)
  join_mutate(x, y,
              by = by, copy = copy, type = "right", suffix = suffix,
              na_matches = na_matches, keep = keep,
              # powerjoin args
              check = check, conflict = conflict, fill = fill
  )
}

#' @export
power_right_join.list <- function(
  x, y = NULL, by = NULL, copy = FALSE, suffix = c(".x", ".y"), keep = NULL,
  na_matches = c("na", "never"),
  check = check_specs(),
  conflict = NULL,
  fill = NULL) {

  if(!is.null(y) &&!is_bare_list(y)) {
    y <- list(y)
  }
  na_matches <- arg_match(na_matches)

  Reduce(function(x,y) power_right_join(
    x, y, by, copy, suffix, keep, na_matches,
    # powerjoin args
    check, conflict, fill
  ), c(x,y))
}


#-------------------------------------------------------------------------------
# inner

#' @export
#' @rdname power_left_join
power_inner_join <- function(
  x, y = NULL, by = NULL, copy = FALSE, suffix = c(".x", ".y"), keep = NULL,
  na_matches = c("na", "never"),
  check = check_specs(),
  conflict = NULL,
  fill = NULL) {
  UseMethod("power_inner_join")
}

# from dplyr 1.0.7
#' @export
power_inner_join.data.frame <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), keep = NULL,
  na_matches = c("na", "never"),
  check = check_specs(),
  conflict = NULL,
  fill =  NULL) {

  if(is_bare_list(y)) {
    x <- list(x)
    res <- power_inner_join.list(
      x, y, by, copy, suffix, keep, na_matches,
      # powerjoin args
      check, conflict, fill
    )
    return(res)
  }
  y <- auto_copy(x, y, copy = copy)
  join_mutate(x, y,
              by = by, copy = copy, type = "inner", suffix = suffix,
              na_matches = na_matches, keep = keep,
              # powerjoin args
              check = check, conflict =conflict
  )
}

#' @export
power_inner_join.list <- function(
  x, y = NULL, by = NULL, copy = FALSE, suffix = c(".x", ".y"), keep = NULL,
  na_matches = c("na", "never"),
  check = check_specs(),
  conflict = NULL,
  fill = NULL) {

  if(!is.null(y) &&!is_bare_list(y)) {
    y <- list(y)
  }
  na_matches <- arg_match(na_matches)

  Reduce(function(x,y) power_inner_join(
    x, y, by, copy, suffix, keep, na_matches,
    # powerjoin args
    check, conflict, fill
  ), c(x,y))
}


#-------------------------------------------------------------------------------
# full

#' @export
#' @rdname power_left_join
power_full_join <- function(
  x, y = NULL, by = NULL, copy = FALSE, suffix = c(".x", ".y"), keep = NULL,
  na_matches = c("na", "never"),
  check = check_specs(),
  conflict = NULL,
  fill = NULL) {
  UseMethod("power_full_join")
}

# from dplyr 1.0.7
#' @export
power_full_join.data.frame <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), keep = NULL,
  na_matches = c("na", "never"),
  check = check_specs(),
  conflict = NULL,
  fill = NULL) {

  if(is_bare_list(y)) {
    x <- list(x)
    res <- power_full_join.list(
      x, y, by, copy, suffix, keep, na_matches,
      # powerjoin args
      check, conflict, fill
    )
    return(res)
  }
  y <- auto_copy(x, y, copy = copy)
  join_mutate(x, y,
              by = by, copy = copy, type = "full", suffix = suffix,
              na_matches = na_matches, keep = keep,
              # powerjoin args
              check = check, conflict = conflict, fill = fill
  )
}

#' @export
power_full_join.list <- function(
  x, y = NULL, by = NULL, copy = FALSE, suffix = c(".x", ".y"), keep = NULL,
  na_matches = c("na", "never"),
  check = check_specs(),
  conflict = NULL,
  fill = NULL) {

  if(!is.null(y) &&!is_bare_list(y)) {
    y <- list(y)
  }
  na_matches <- arg_match(na_matches)

  Reduce(function(x,y) power_full_join(
    x, y, by, copy, suffix, keep, na_matches,
    # powerjoin args
    check, conflict, fill
  ), c(x,y))
}
