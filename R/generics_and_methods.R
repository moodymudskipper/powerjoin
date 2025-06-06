
#-------------------------------------------------------------------------------
# left

#' Power joins
#'
#' @param by As in \pkg{dplyr}, but extended so user can supply a formula or a list
#'  of character and formulas. Formulas are used for fuzzy joins, see dedicated
#'  section below.
#' @inheritParams dplyr::left_join
#' @param check A list created with `check_specs()`
#' @param conflict A function, formula, the special value amongst `"patch"`,
#'   or a named list of such items. If the LHS of the formula is `rw` the rhs will
#'   be applied rowwise. Note that the columns will be subset with `[` so for
#'   list columns `.x` or `.y` will refer to length 1 lists and you might sometimes
#'   need `.x[[1]]` or `.y[[1]]`.
#' @param fill Values used to replace missing values originating in unmatched keys,
#'   or a named list of such items.
#' @param keep A boolean for compatibility with \pkg{dplyr}, or a value among "left",
#' "right", "both", "none" or "default". See dedicated section below.
#' @param copy Ignored at the moment because \pkg{powerjoin} doesn't support databases
#'
#' @section `keep` argument values:
#'
#' - `NULL` (default) : merge keys and name them as the left table's keys, and
#'   keep columns used for fuzzy joins from both tables
#' - `left` : keep only key columns for left table
#' - `right`: keep only key columns for right table
#' - `both` or `TRUE`: keep key columns from both tables, adding suffix if relevant
#' - `none` : drop all key columns from the output
#' - `FALSE` : merge keys and name them as the left table's keys, maps to `none` for fuzzy joins
#'
#' @section fuzzy joins:
#'
#' To specify fuzzy matching conditions we use formulas in which we refer to
#' the columns from the left side data frame using `.x` and the right side data frame
#' using `.y`, for instance `by = ~ .x$col1 > .y$col2`.
#'
#' We can specify several conditions and even mix equi condition with fuzzy condition,
#' for instance `by = c(col1 = "col2", ~ .x$col3 > .y$col4)`
#'
#' To fuzzy match strings we can leverage the functions from the \pkg{stringr}
#' package since they are vectorized on all main arguments,
#' for instance to match observations where `col1` contains `col1` we can attach
#' \pkg{stringr} and do `by =  ~ str_detect(.x$col1, fixed(.y$col2))`.
#'
#' Another useful function is `stringdist` from the \pkg{stringdist} package to match
#' strings that are close enough, for instance `by = ~ stringdist::stringdist(.x$a,.y$a) < 2`
#'
#' We can also define a new column computed during the fuzzy matching, using the
#' arrow assignment operator, for instance : `by = ~ .x$col1 > (mysum <- .y$col2 + .y$col3)`
#'
#' When the `by` condition evaluates to `NA` the observation is dismissed. This makes
#' `by = c(a = "b")` slightly different from `by = ~ .x$a == .y$b` when `na_matches`
#' is `"na"` (the default). To be able to match `NA` with `NA` in fuzzy matching condition
#' we can use the `%==%` operator (bone operator), defined in this package.
#'
#' @return A data frame
#' @export
#' @examples
#' # See README for a more verbose version
#' library(tibble)
#' male_penguins <- tribble(
#'   ~name,    ~species,     ~island, ~flipper_length_mm, ~body_mass_g,
#'   "Giordan",    "Gentoo",    "Biscoe",               222L,        5250L,
#'   "Lynden",    "Adelie", "Torgersen",               190L,        3900L,
#'   "Reiner",    "Adelie",     "Dream",               185L,        3650L
#' )
#'
#' female_penguins <- tribble(
#'   ~name,    ~species,  ~island, ~flipper_length_mm, ~body_mass_g,
#'   "Alonda",    "Gentoo", "Biscoe",               211,        4500L,
#'   "Ola",    "Adelie",  "Dream",               190,        3600L,
#'   "Mishayla",    "Gentoo", "Biscoe",               215,        4750L,
#' )
#'
#' # apply different checks
#' power_inner_join(
#'   male_penguins[c("species", "island")],
#'   female_penguins[c("species", "island")],
#'   check = check_specs(implicit_keys = "ignore", duplicate_keys_right = "inform")
#' )
#'
#' df1 <- tibble(id = 1:3, value = c(10, NA, 30))
#' df2 <- tibble(id = 2:4, value = c(22, 32, 42))
#'
#' # handle conflicted columns when joining
#' power_left_join(df1, df2, by = "id", conflict = `+`)
#'
#' # the most frequent use case is to coalesce
#' power_left_join(df1, df2, by = "id", conflict = coalesce_xy)
#' power_left_join(df1, df2, by = "id", conflict = coalesce_yx)
#'
#' # the conflict function is applied colwise by default!
#' power_left_join(df1, df2, by = "id", conflict = ~ sum(.x, .y, na.rm = TRUE))
#'
#' # apply conflict function rowwise
#' power_left_join(df1, df2, by = "id", conflict = rw ~ sum(.x, .y, na.rm = TRUE))
#'
#' # subset columns without repeating keys
#' power_inner_join(
#'   male_penguins %>% select_keys_and(name),
#'   female_penguins %>% select_keys_and(female_name = name),
#'   by = c("species", "island")
#' )
#'
#' # semi join
#' power_inner_join(
#'   male_penguins,
#'   female_penguins %>% select_keys_and(),
#'   by = c("species", "island")
#' )
#'
#' # agregate without repeating keys
#' power_left_join(
#'   male_penguins %>% summarize_by_keys(male_weight = mean(body_mass_g)),
#'   female_penguins %>% summarize_by_keys(female_weight = mean(body_mass_g)),
#'   by = c("species", "island")
#' )
#'
#' # pack auxiliary colums without repeating keys
#' power_left_join(
#'   male_penguins %>% pack_along_keys(name = "m"),
#'   female_penguins %>% pack_along_keys(name = "f"),
#'   by = c("species", "island")
#' )
#'
#' # fuzzy join
#' power_inner_join(
#'   male_penguins %>% select_keys_and(male_name = name),
#'   female_penguins %>% select_keys_and(female_name = name),
#'   by = c(~.x$flipper_length_mm < .y$flipper_length_mm, ~.x$body_mass_g > .y$body_mass_g)
#' )
#'
#' # fuzzy + equi join
#' power_inner_join(
#'   male_penguins %>% select_keys_and(male_name = name),
#'   female_penguins %>% select_keys_and(female_name = name),
#'   by = c("island", ~.x$flipper_length_mm > .y$flipper_length_mm)
#' )
#'
#' # define new column without repeating computation
#' power_inner_join(
#'   male_penguins %>% select_keys_and(male_name = name),
#'   female_penguins %>% select_keys_and(female_name = name),
#'   by = ~ (mass_ratio <- .y$body_mass_g / .x$body_mass_g) > 1.2
#' )
#' power_inner_join(
#'   male_penguins %>% select_keys_and(male_name = name),
#'   female_penguins %>% select_keys_and(female_name = name),
#'   by = ~ (mass_ratio <- .y$body_mass_g / .x$body_mass_g) > 1.2,
#'   keep = "none"
#' )
#'
#' # fill unmatched values
#' df1 <- tibble(id = 1:3)
#' df2 <- tibble(id = 1:2, value2 = c(2, NA), value3 = c(NA, 3))
#' power_left_join(df1, df2, by = "id", fill = 0)
#' power_left_join(df1, df2, by = "id", fill = list(value2 = 0))
#'
#' # join recursively
#' df1 <- tibble(id = 1, a = "foo")
#' df2 <- tibble(id = 1, b = "bar")
#' df3 <- tibble(id = 1, c = "baz")
#' power_left_join(list(df1, df2, df3), by = "id")
#' power_left_join(df1, list(df2, df3), by = "id")
#'
power_left_join <- function(
  x, y = NULL, by = NULL, copy = FALSE, suffix = c(".x", ".y"), keep = NULL,
  na_matches = c("na", "never"),
  check = check_specs(),
  conflict = NULL,
  fill = NULL) {
  if (!inherits(check, "powerjoin_check")) abort("The `check` argument should be built with `check_specs()`")
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
              check = check, conflict = conflict, fill = fill,
              env = parent.frame()
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
  if (!inherits(check, "powerjoin_check")) abort("The `check` argument should be built with `check_specs()`")
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
              check = check, conflict = conflict, fill = fill,
              env = parent.frame()
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
  if (!inherits(check, "powerjoin_check")) abort("The `check` argument should be built with `check_specs()`")
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
              check = check, conflict = conflict,
              env = parent.frame()
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
  if (!inherits(check, "powerjoin_check")) abort("The `check` argument should be built with `check_specs()`")
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
              check = check, conflict = conflict, fill = fill,
              env = parent.frame()
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
