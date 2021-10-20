
#-------------------------------------------------------------------------------
# left

#' Power joins
#'
#' @inheritParams dplyr::left_join
#' @param check A list created with `pj_check()`
#' @param conflict WIP
#' @param fuzzy WIP
#' @export
power_left_join <- function(
  x, y = NULL, by = NULL, copy = FALSE, suffix = c(".x", ".y"), keep = FALSE,
  na_matches = c("na", "never"),
  check = pj_check(),
  conflict = NULL,
  fuzzy = NULL) {
  UseMethod("power_left_join")
}

# from dplyr 1.0.7
#' @export
power_left_join.data.frame <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), keep = FALSE,
  na_matches = c("na", "never"),
  check = pj_check(),
  conflict = NULL,
  fuzzy = NULL) {

  if(is_bare_list(y)) {
    x <- list(x)
    res <- power_left_join.list(
      x, y, by, copy, type, suffix, keep, na_matches,
      # powerjoin args
      check, conflict, fuzzy
    )
    return(res)
  }
  y <- auto_copy(x, y, copy = copy)
  join_mutate(x, y,
              by = by, copy = copy, type = "left", suffix = suffix,
              na_matches = na_matches, keep = keep,
              # powerjoin args
              check = check, conflict =conflict, fuzzy = fuzzy
  )
}

#' @export
power_left_join.list <- function(
  x, y = NULL, by = NULL, copy = FALSE, suffix = c(".x", ".y"), keep = FALSE,
  na_matches = c("na", "never"),
  check = pj_check(),
  conflict = NULL,
  fuzzy = NULL) {

  if(!is.null(y) &&!is_bare_list(y)) {
    y <- list(y)
  }
  na_matches <- arg_match(na_matches)

  Reduce(function(x,y) power_left_join(
    x, y, by, copy, type, suffix, keep, na_matches,
    # powerjoin args
    check, conflict, fuzzy
  ), c(x,y))
}


#-------------------------------------------------------------------------------
# right

#' @export
#' @rdname power_left_join
power_right_join <- function(
  x, y = NULL, by = NULL, copy = FALSE, suffix = c(".x", ".y"), keep = FALSE,
  na_matches = c("na", "never"),
  check = pj_check(),
  conflict = NULL,
  fuzzy = NULL) {
  UseMethod("power_right_join")
}

# from dplyr 1.0.7
#' @export
power_right_join.data.frame <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), keep = FALSE,
  na_matches = c("na", "never"),
  check = pj_check(),
  conflict = NULL,
  fuzzy = NULL) {

  if(is_bare_list(y)) {
    x <- list(x)
    res <- power_right_join.list(
      x, y, by, copy, type, suffix, keep, na_matches,
      # powerjoin args
      check, conflict, fuzzy
    )
    return(res)
  }
  y <- auto_copy(x, y, copy = copy)
  join_mutate(x, y,
              by = by, copy = copy, type = "right", suffix = suffix,
              na_matches = na_matches, keep = keep,
              # powerjoin args
              check = check, conflict =conflict, fuzzy = fuzzy
  )
}

#' @export
power_right_join.list <- function(
  x, y = NULL, by = NULL, copy = FALSE, suffix = c(".x", ".y"), keep = FALSE,
  na_matches = c("na", "never"),
  check = pj_check(),
  conflict = NULL,
  fuzzy = NULL) {

  if(!is.null(y) &&!is_bare_list(y)) {
    y <- list(y)
  }
  na_matches <- arg_match(na_matches)

  Reduce(function(x,y) power_right_join(
    x, y, by, copy, type, suffix, keep, na_matches,
    # powerjoin args
    check, conflict, fuzzy
  ), c(x,y))
}


#-------------------------------------------------------------------------------
# inner

#' @export
#' @rdname power_left_join
power_inner_join <- function(
  x, y = NULL, by = NULL, copy = FALSE, suffix = c(".x", ".y"), keep = FALSE,
  na_matches = c("na", "never"),
  check = pj_check(),
  conflict = NULL,
  fuzzy = NULL) {
  UseMethod("power_inner_join")
}

# from dplyr 1.0.7
#' @export
power_inner_join.data.frame <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), keep = FALSE,
  na_matches = c("na", "never"),
  check = pj_check(),
  conflict = NULL,
  fuzzy = NULL) {

  if(is_bare_list(y)) {
    x <- list(x)
    res <- power_inner_join.list(
      x, y, by, copy, type, suffix, keep, na_matches,
      # powerjoin args
      check, conflict, fuzzy
    )
    return(res)
  }
  y <- auto_copy(x, y, copy = copy)
  join_mutate(x, y,
              by = by, copy = copy, type = "inner", suffix = suffix,
              na_matches = na_matches, keep = keep,
              # powerjoin args
              check = check, conflict =conflict, fuzzy = fuzzy
  )
}

#' @export
power_inner_join.list <- function(
  x, y = NULL, by = NULL, copy = FALSE, suffix = c(".x", ".y"), keep = FALSE,
  na_matches = c("na", "never"),
  check = pj_check(),
  conflict = NULL,
  fuzzy = NULL) {

  if(!is.null(y) &&!is_bare_list(y)) {
    y <- list(y)
  }
  na_matches <- arg_match(na_matches)

  Reduce(function(x,y) power_inner_join(
    x, y, by, copy, type, suffix, keep, na_matches,
    # powerjoin args
    check, conflict, fuzzy
  ), c(x,y))
}


#-------------------------------------------------------------------------------
# full

#' @export
#' @rdname power_left_join
power_full_join <- function(
  x, y = NULL, by = NULL, copy = FALSE, suffix = c(".x", ".y"), keep = FALSE,
  na_matches = c("na", "never"),
  check = pj_check(),
  conflict = NULL,
  fuzzy = NULL) {
  UseMethod("power_full_join")
}

# from dplyr 1.0.7
#' @export
power_full_join.data.frame <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), keep = FALSE,
  na_matches = c("na", "never"),
  check = pj_check(),
  conflict = NULL,
  fuzzy = NULL) {

  if(is_bare_list(y)) {
    x <- list(x)
    res <- power_full_join.list(
      x, y, by, copy, type, suffix, keep, na_matches,
      # powerjoin args
      check, conflict, fuzzy
    )
    return(res)
  }
  y <- auto_copy(x, y, copy = copy)
  join_mutate(x, y,
              by = by, copy = copy, type = "full", suffix = suffix,
              na_matches = na_matches, keep = keep,
              # powerjoin args
              check = check, conflict =conflict, fuzzy = fuzzy
  )
}

#' @export
power_full_join.list <- function(
  x, y = NULL, by = NULL, copy = FALSE, suffix = c(".x", ".y"), keep = FALSE,
  na_matches = c("na", "never"),
  check = pj_check(),
  conflict = NULL,
  fuzzy = NULL) {

  if(!is.null(y) &&!is_bare_list(y)) {
    y <- list(y)
  }
  na_matches <- arg_match(na_matches)

  Reduce(function(x,y) power_full_join(
    x, y, by, copy, type, suffix, keep, na_matches,
    # powerjoin args
    check, conflict, fuzzy
  ), c(x,y))
}
