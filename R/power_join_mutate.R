join_mutate <- function(x, y, by, copy, type, suffix = c(".x", ".y"), na_matches = c("na", "never"),
                        keep = FALSE,
                        # powerjoin args
                        check = pj_check(),
                        conflict = NULL,
                        fuzzy = NULL) {
  na_matches <- arg_match(na_matches)

  if(is_bare_list(x) || is_bare_list(y)) {
    if(!is_bare_list(x)) {
      x <- list(x)
    } else if(!is.null(y) &&!is_bare_list(y)) {
      y <- list(y)
    }
    Reduce(function(x,y) join_mutate_impl(
      x, y, by, copy, type, suffix, na_matches,
      keep,
      # powerjoin args
      check,
      conflict,
      fuzzy
    ), c(x,y))
  } else {
    join_mutate_impl(
      x, y, by, copy, type, suffix, na_matches,
      keep,
      # powerjoin args
      check,
      conflict,
      fuzzy
    )
  }
}

# Adapted from join_mutate in dplyr 1.0.7
join_mutate <- function(
  # dplyr args
  x, y, by, copy, type, suffix = c(".x", ".y"), na_matches = c("na", "never"),
  keep = FALSE,
  # powerjoin args
  check = pj_check(),
  conflict = NULL,
  fuzzy = NULL) {
  # if(is_formula(x)) {
  #   x_bkp <- x
  #   x <- eval(x[[2]], environment(x))
  # }
  # if(is_formula(y)) {
  #   y_bkp <- y
  #   y <- eval(y[[2]], environment(y))
  # }
  #-----------------------------------------------------------------------------
  # implicit_by
  if(check[["implicit_by"]] %in% "abort") {
    abort("`by`is `NULL`, join columns should be explicit")
  }
  #-----------------------------------------------------------------------------
  # inconsistent_type
  # by needs to be explicited first
  # check_inconsistent_type(x, y, by, check[["inconsistent_type"]])
  #-----------------------------------------------------------------------------

  vars <- join_cols(tbl_vars(x), tbl_vars(y),
                    by = by, suffix = suffix,
                    keep = keep,
                    # powerjoin args
                    check = check
  )

  na_equal <- check_na_matches(na_matches)
  x_in <- as_tibble(x, .name_repair = "minimal")
  y_in <- as_tibble(y, .name_repair = "minimal")

  #-----------------------------------------------------------------------------
  # duplicate_keys_left
  if(!is.na(check[["duplicate_keys_left"]]) &&
     n_distinct(x[vars$x$key]) != nrow(x)) {
    fun <- getFromNamespace(check[["duplicate_keys_left"]], "rlang")
    dupes <- x[vars$x$key]
    dupes <- dupes[duplicated(dupes),]
    msg <- paste0(
      "Keys in the left table have duplicates:\n",
      paste(capture.output(dupes), collapse = "\n")
    )
    fun(msg)
  }
  #-----------------------------------------------------------------------------
  # duplicate_keys_right
  if(!is.na(check[["duplicate_keys_right"]]) &&
     n_distinct(y[vars$y$key]) != nrow(y)) {
    fun <- getFromNamespace(check[["duplicate_keys_right"]], "rlang")
    dupes <- y[vars$y$key]
    dupes <- dupes[duplicated(dupes),]
    msg <- paste0(
      "Keys in the right table have duplicates:\n",
      paste(capture.output(dupes), collapse = "\n")
    )
    fun(msg)
  }
  #-----------------------------------------------------------------------------
  # missing_key_combination_left
  if(!is.na(check[["missing_key_combination_left"]]) &&
     n_distinct(x[vars$x$key]) != prod(vapply(x[vars$x$key], n_distinct, numeric(1)))) {
    fun <- getFromNamespace(check[["missing_key_combination_left"]], "rlang")
    all_combos <- exec(
      "expand.grid",
      !!! lapply(x[vars$x$key], unique),
      stringsAsFactors = FALSE)
    missing_combos <- setdiff(all_combos, x[vars$x$key])
    missing_combos <- as_tibble(missing_combos)
    msg <- paste0(
      "Keys in the left table have missing combinations:\n",
      paste(capture.output(missing_combos), collapse = "\n")
    )
    fun(msg)
  }
  #-----------------------------------------------------------------------------
  # missing_key_combination_right
  if(!is.na(check[["missing_key_combination_right"]]) &&
     n_distinct(y[vars$y$key]) != prod(vapply(y[vars$y$key], n_distinct, numeric(1)))) {
    fun <- getFromNamespace(check[["missing_key_combination_right"]], "rlang")
    all_combos <- exec(
      "expand.grid",
      !!! lapply(y[vars$y$key], unique),
      stringsAsFactors = FALSE)
    missing_combos <- setdiff(all_combos, y[vars$y$key])
    missing_combos <- as_tibble(missing_combos)
    msg <- paste0(
      "Keys in the right table have missing combinations:\n",
      paste(capture.output(missing_combos), collapse = "\n")
    )
    fun(msg)
  }
  #-----------------------------------------------------------------------------
  # inconsistent_factor_levels
  if(!is.na(check[["inconsistent_factor_levels"]])) {
    fun <- getFromNamespace(check[["inconsistent_factor_levels"]], "rlang")
    if(check[["inconsistent_factor_levels"]] == "abort") {
      msg <- mapply(function(x, y, x_nm, y_nm) {
        if(is.factor(x) && is.factor(y) && !setequal(levels(x), levels(y))) {
          if(x_nm == y_nm)
            sprintf("`%s` has different factor levels in the left and right tables", x_nm)
          else
            sprintf("`%s` and `%s` (in resp. left and right table) have different factor levels", x_nm, y_nm)
        }}, x_in[vars$x$key], y_in[vars$y$key], names(x_in[vars$x$key]), names(y_in[vars$y$key]))
    } else {
      msg <- mapply(function(x, y, x_nm, y_nm) {
        if(is.factor(x) && is.factor(y) && !setequal(levels(x), levels(y))) {
          if(x_nm == y_nm)
            sprintf("`%s` has different factor levels in the left and right tables, coercing to character vector", x_nm)
          else
            sprintf("`%s` and `%s` (in resp. the left and right table) have different factor levels, coercing to character vector", x_nm, y_nm)
        }}, x_in[vars$x$key], y_in[vars$y$key], names(x_in[vars$x$key]), names(y_in[vars$y$key]))
    }
    msg <- msg[lengths(msg)>0]
    msg <- paste(msg, collapse = "\n")
    fun(msg)
  }
  #-----------------------------------------------------------------------------
  # type
  if(!is.na(check[["inconsistent_type"]])) {
    fun <- getFromNamespace(check[["inconsistent_type"]], "rlang")
    msg <- mapply(function(x, y, x_nm, y_nm) {
      if(!identical(typeof(x), typeof(y)) || !identical(class(x), class(y))) {
        if(x_nm == y_nm)
          sprintf("`%s` has a different type or class in the left and right tables", x_nm)
        else
          sprintf("`%s` and `%s` (in resp. left and right table) have different types or classes", x_nm, y_nm)
      }}, x_in[vars$x$key], y_in[vars$y$key], names(x_in[vars$x$key]), names(y_in[vars$y$key]))
    msg <- msg[lengths(msg)>0]
    if(length(msg)) {
      msg <- paste(msg, collapse = "\n")
      fun(msg)
    }
  }
  #-----------------------------------------------------------------------------
  # original dplyr code
  x_key <- set_names(x_in[vars$x$key], names(vars$x$key))
  y_key <- set_names(y_in[vars$y$key], names(vars$y$key))
  rows <- join_rows(x_key, y_key, type = type, na_equal = na_equal)
  #-----------------------------------------------------------------------------
  # unmatched_keys_left
  if(!is.na(check[["unmatched_keys_left"]]) && length(rows$x_unmatched)) {
    fun <- getFromNamespace(check[["unmatched_keys_left"]], "rlang")
    unmatched_combos <- distinct(x_in[rows$x_unmatched, vars$x$key])
    msg <- paste0(
      "Keys in the left table have unmatched combinations:\n",
      paste(capture.output(unmatched_combos), collapse = "\n")
    )
    fun(msg)
  }
  #-----------------------------------------------------------------------------
  # unmatched_keys_right
  if(!is.na(check[["unmatched_keys_right"]]) && length(rows$y_unmatched)) {
    fun <- getFromNamespace(check[["unmatched_keys_right"]], "rlang")
    unmatched_combos <- distinct(y_in[rows$y_unmatched, vars$y$key])
    msg <- paste0(
      "Keys in the right table have unmatched combinations:\n",
      paste(capture.output(unmatched_combos), collapse = "\n")
    )
    fun(msg)
  }
  #-----------------------------------------------------------------------------
  # original dplyr code
  x_out <- set_names(x_in[vars$x$out], names(vars$x$out))
  y_out <- set_names(y_in[vars$y$out], names(vars$y$out))
  if (length(rows$y_extra) > 0L) {
    x_slicer <- c(rows$x, rep_along(rows$y_extra, NA_integer_))
    y_slicer <- c(rows$y, rows$y_extra)
  } else {
    x_slicer <- rows$x
    y_slicer <- rows$y
  }
  out <- vec_slice(x_out, x_slicer)
  out[names(y_out)] <- vec_slice(y_out, y_slicer)
  if (!keep) {
    key_type <- vec_ptype_common(x_key, y_key)
    out[names(x_key)] <- vec_cast(out[names(x_key)], key_type)
    if (length(rows$y_extra) > 0L) {
      new_rows <- length(rows$x) + seq_along(rows$y_extra)
      out[new_rows, names(y_key)] <- vec_cast(vec_slice(
        y_key,
        rows$y_extra
      ), key_type)
    }
  }
  dplyr_reconstruct(out, x)
}


# Adapted from join_mutate in dplyr 1.0.7
join_cols <- function(
  x_names, y_names, by = NULL, suffix = c(".x", ".y"), keep = FALSE,
  # arg from powerjoin
  check) {
  # original dplyr code
  check_duplicate_vars(x_names, "x")
  check_duplicate_vars(y_names, "y")
  by <- standardise_join_by(by, x_names = x_names, y_names = y_names,
                            # arg from powerjoin
                            check = check)
  intersect_ <- intersect(x_names, setdiff(y_names, by$y))
  #-----------------------------------------------------------------------------
  #   column_conflict
  if(!is.na(check[["column_conflict"]]) && length(intersect_)) {
    fun <- getFromNamespace(check[["implicit_by"]], "rlang")
    if(check[["column_conflict"]] == "abort") {
      msg <- paste("The following columns are ambiguous: ", toString(intersect_))
      abort(msg)
    } else {
      msg <- paste(
        "The following columns are ambiguous and will be prefixed: ",
        toString(intersect_))
      fun(msg)
    }
  }
  #-----------------------------------------------------------------------------
  # original dplyr code
  suffix <- standardise_join_suffix(suffix)
  x_by <- set_names(match(by$x, x_names), by$x)
  y_by <- set_names(match(by$y, y_names), by$x)
  x_loc <- seq_along(x_names)
  names(x_loc) <- x_names
  if (!keep) {
    y_aux <- setdiff(y_names, c(by$x, if (!keep) by$y))
    x_is_aux <- !x_names %in% by$x
    names(x_loc)[x_is_aux] <- add_suffixes(
      x_names[x_is_aux],
      c(by$x, y_aux), suffix$x
    )
  } else {
    names(x_loc) <- add_suffixes(x_names, y_names, suffix$x)
  }
  y_loc <- seq_along(y_names)
  names(y_loc) <- add_suffixes(y_names, x_names, suffix$y)
  if (!keep) {
    y_loc <- y_loc[!y_names %in% by$y]
  }
  list(x = list(key = x_by, out = x_loc), y = list(
    key = y_by,
    out = y_loc
  ))
}

# Adapted from join_mutate in dplyr 1.0.7
standardise_join_by <- function(
  by, x_names, y_names,
  # arg from powerjoin
  check) {
  if (is.null(by)) {
    by <- intersect(x_names, y_names)
    if (length(by) == 0) {
      abort(c("`by` must be supplied when `x` and `y` have no common variables.",
              i = "use by = character()` to perform a cross-join."
      ))
    }
    by_quoted <- encodeString(by, quote = "\"")
    if (length(by_quoted) == 1L) {
      by_code <- by_quoted
    } else {
      by_code <- paste0(
        "c(", paste(by_quoted, collapse = ", "),
        ")"
      )
    }
    #---------------------------------------------------------------------------
    # implicit_by
    if(!is.na(check[["implicit_by"]])) {
      fun <- getFromNamespace(check[["implicit_by"]], "rlang")
      fun(paste0("Joining, by = ", by_code))
    }
    # original dplyr code
    # inform(paste0("Joining, by = ", by_code))
    #---------------------------------------------------------------------------
    by <- list(x = by, y = by)
  } else if (is.character(by)) {
    by_x <- names(by) %||% by
    by_y <- unname(by)
    by_x[by_x == ""] <- by_y[by_x == ""]
    by <- list(x = by_x, y = by_y)
  } else if (is.list(by)) {
    by <- by[c("x", "y")]
  } else {
    bad_args("by", "must be a (named) character vector, list, or NULL, not {friendly_type_of(by)}.")
  }
  check_join_vars(by$x, x_names)
  check_join_vars(by$y, y_names)
  by
}


# Adapted from join_rows in dplyr 1.0.7
join_rows <- function(x_key, y_key, type = c("inner", "left", "right", "full"),
                        na_equal = TRUE) {
  type <- arg_match(type)
  y_split <- vec_group_loc(y_key)
  tryCatch(matches <- vec_match(x_key, y_split$key, na_equal = na_equal),
           vctrs_error_incompatible_type = function(cnd) {
             rx <- "^[^$]+[$]"
             x_name <- sub(rx, "", cnd$x_arg)
             y_name <- sub(rx, "", cnd$y_arg)
             abort(c(glue("Can't join on `x${x_name}` x `y${y_name}` because of incompatible types."),
                     i = glue("`x${x_name}` is of type <{x_type}>>.",
                              x_type = vec_ptype_full(cnd$x)
                     ), i = glue("`y${y_name}` is of type <{y_type}>>.",
                                 y_type = vec_ptype_full(cnd$y)
                     )
             ))
           }
  )
  y_loc <- y_split$loc[matches]
  x_loc <- seq_len(vec_size(x_key)) # moved up compared to dplyr code
  #-----------------------------------------------------------------------------
  x_unmatched <- x_loc[!lengths(y_loc)]
  y_unmatched <- setdiff(seq(nrow(y_key)), unlist(y_loc))
  #-----------------------------------------------------------------------------
  if (type == "left" || type == "full") {
    if (anyNA(matches)) {
      y_loc <- vec_assign(
        y_loc, vec_equal_na(matches),
        list(NA_integer_)
      )
    }
  }
  x_loc <- rep(x_loc, lengths(y_loc))
  y_loc <- index_flatten(y_loc)
  y_extra <- integer()
  if (type == "right" || type == "full") {
    miss_x <- !vec_in(y_key, x_key, na_equal = na_equal)
    if (!na_equal) {
      miss_x[is.na(miss_x)] <- TRUE
    }
    if (any(miss_x)) {
      y_extra <- seq_len(vec_size(y_key))[miss_x]
    }
  }
  list(x = x_loc, y = y_loc, y_extra = y_extra,
       x_unmatched = x_unmatched, y_unmatched = y_unmatched)
}


# Adapted from add_suffixes in dplyr 1.0.7
add_suffixes <- function(x, y, suffix) {
  if (identical(suffix, "")) {
    return(x)
  }
  out <- rep_along(x, na_chr)
  for (i in seq_along(x)) {
    nm <- x[[i]]
    while (nm %in% y || nm %in% out[seq_len(i - 1)]) {
      nm <- paste0(nm, suffix)
    }
    out[[i]] <- nm
  }
  out
}
