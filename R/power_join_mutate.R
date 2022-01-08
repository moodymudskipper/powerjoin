# Adapted from join_mutate in dplyr 1.0.7
join_mutate <- function(
  # dplyr args
  x, y, by, copy, type, suffix = c(".x", ".y"), na_matches = c("na", "never"),
  keep = NULL,
  # powerjoin args
  check = check_specs(),
  conflict = NULL,
  fill = NULL) {
  check <- complete_specs(check)
  #-----------------------------------------------------------------------------
  # checks on raw data
  check_implicit_keys1(by, check)
  check_grouped(x, y, check)
  #-----------------------------------------------------------------------------
  # transform `by` to list of `x`, `y` and more if fuzzy
  by <- preprocess_by(tbl_vars(x), tbl_vars(y), by = by, check = check)
  #-----------------------------------------------------------------------------
  # check keep's values and assess right default if relevant
  keep <- match_keep(keep, by$fuzzy)
  #-----------------------------------------------------------------------------
  # preprocess the data inputs
  x <- preprocess(x, by$x)
  y <- preprocess(y, by$y)
  #-----------------------------------------------------------------------------
  # checks on preprocessed data
  check_column_conflict_extra(tbl_vars(x), tbl_vars(y), by, keep, check)
  check_duplicate_keys_left(x, by$x, check)
  check_duplicate_keys_right(y, by$y, check)
  check_missing_key_combination_left(x, by$x, check)
  check_missing_key_combination_right(y, by$y, check)
  check_inconsistent_factor_levels(x, y, by, check)
  check_inconsistent_type(x, y, by, check)
  check_na_keys(x, y, by, check)
  #-----------------------------------------------------------------------------
  # dplyr original code
  na_equal <- check_na_matches(na_matches)
  x_in <- as_tibble(x, .name_repair = "minimal")
  y_in <- as_tibble(y, .name_repair = "minimal")
  #-----------------------------------------------------------------------------
  # We need conflicted cols for the conflict arg and the column_conflict check
  # This should include columns created through fuzzy matches
  conflicted_cols <- conflicted_columns(tbl_vars(x), tbl_vars(y), by, keep, conflict)
  if(is.list(conflict)) conflict <- conflict[conflicted_cols$handled]
  check_column_conflict(conflicted_cols, check)
  #-----------------------------------------------------------------------------
  # modified dplyr code
  vars <- join_cols(tbl_vars(x), tbl_vars(y),
                    by = by, suffix = suffix,
                    keep = keep,
                    # powerjoin args
                    check = check,
                    conflicted_cols = conflicted_cols
  )
  # give potentially suffixed names
  x_key <- set_names(x_in[vars$x$key], names(vars$x$key))
  y_key <- set_names(y_in[vars$y$key], names(vars$y$key))
  if(by$fuzzy) {
    rows <- join_rows_fuzzy(x, y, by, by$multi_match_fun, type = type)
  } else {
    rows <- join_rows(x_key, y_key, type = type, na_equal = na_equal)
  }

  #-----------------------------------------------------------------------------
  # powerjoin checks
  check_unmatched_keys_left(x, y, by$x, rows, check)
  check_unmatched_keys_right(x, y, by$y, rows, check)
  #-----------------------------------------------------------------------------
  # rename unhandled conflicted columns
  x_out <- set_names(x_in[vars$x$out], names(vars$x$out))
  y_out <- set_names(y_in[vars$y$out], names(vars$y$out))
  # build slicers
  if (length(rows$y_extra) > 0L) {
    x_slicer <- c(rows$x, rep_along(rows$y_extra, NA_integer_))
    y_slicer <- c(rows$y, rows$y_extra)
  } else {
    x_slicer <- rows$x
    y_slicer <- rows$y
  }
  # slice left table
  x_sliced <- vec_slice(x_out[!names(x_out) %in% conflicted_cols$handled], x_slicer)
  # slice right table
  y_sliced <- vec_slice(y_out[!names(y_out) %in% conflicted_cols$handled], y_slicer)

  out <- bind_cols(x_sliced, y_sliced) # and conflicted_sliced
  #-----------------------------------------------------------------------------
  # handle conflicts
  conflicted_data <- list(x = x_out[conflicted_cols$handled],
                          y = y_out[conflicted_cols$handled])
  out <- handle_conflicts(out, x_slicer, y_slicer, conflicted_data, conflict)
  #-----------------------------------------------------------------------------
  # fill y side
  if(!is.null(fill)) {
    if(is.list(fill)) {
      for (nm in intersect(names(fill), names(y_out))) {
          out[is.na(y_slicer), nm] <- fill[[nm]]
      }
    } else {
      out[is.na(y_slicer), names(y_out)] <- fill
    }
  }
  #-----------------------------------------------------------------------------
  # add extra columns, this might create a conflict
  if(by$fuzzy) {
    out <- bind_cols(out, rows$extra_cols)
  }
  #-----------------------------------------------------------------------------
  # we might want to have different behavior for "default" and "left"
  # by default in a non fuzzy join by cols are merged, so even though we keep the
  # left side name, the values might be built from both sides (right or full join)
  # to be consistent it is coerced to common type in any case.

  if (keep %in% c("default", "default_fuzzy")) {
    if (keep == "default_fuzzy") {
      # overwrite x_key and y_key so equi key columns can be combined
      x_key <- x_key[names(by$equi_keys)]
      y_key <- y_key[names(by$equi_keys)]
    }
    key_type <- vec_ptype_common(x_key, y_key)
    # convert keys to common type
    out[names(x_key)] <- vec_cast(out[names(x_key)], key_type)
    # if we have a right or full join with unmatched keys in right table
    if (length(rows$y_extra) > 0L) {
      new_rows <- length(rows$x) + seq_along(rows$y_extra)
      out[new_rows, names(y_key)] <- vec_cast(vec_slice(
        y_key,
        rows$y_extra
      ), key_type)

      # fill should be treated even if fuzzy, it should work on final names (potentially suffixed)
      # not sure what we're doing below
      # fill x side
      if(!is.null(fill)) {
        x_nms <- setdiff(names(out), c(names(y_out), names(y_key)))
        if(is.list(fill)) {
          for (nm in intersect(names(fill), x_nms)) {
            out[is.na(x_slicer), nm] <- fill[[nm]]
          }
        } else {
        out[new_rows, x_nms] <- fill
        }
      }

    }
  }
  dplyr_reconstruct(out, x)
}

conflicted_columns <- function(x_names, y_names, by, keep, conflict) {
  # key columns that are merged are not candidate for conflict
  # the default behavior should depend on fuzziness!
  conflicted <- switch(
    keep,
    left =,
    default = intersect(x_names, setdiff(y_names, by$y)),
    right = intersect(setdiff(x_names, by$x), y_names),
    default_fuzzy =,
    both = intersect(x_names, y_names),
    none = intersect(setdiff(x_names, by$x), setdiff(y_names, by$y)))
  if(is.null(conflict)) return(list(unhandled = conflicted))
  nms <- names(conflict)
  if(is.null(nms)) return(list(handled = conflicted))
  unused <- setdiff(nms, conflicted)
  if(length(unused)) {
    warn(paste("Some conflict conditions are not used, these are not conflicted columns:",
               toString(paste0("'", unused, "'"))))
  }
  handled <- intersect(nms, conflicted)
  list(
    handled = handled,
    unhandled = setdiff(conflicted, handled)
  )
}

match_keep <- function(keep, fuzzy) {
  # if we're clean vars has by, by_fuzzy, x_aux, x_conflict, for now we can't have robust behavior
  # when mixing equi and fuzzy

  # do we need keep = "equi" to keep only "equi" columns" when we mix ?

  # default is to fuse equi joins and keep both sides for fuzzy joins
  if(is.null(keep)) {
    if (fuzzy) return("default_fuzzy")
    return("default")
  }
  # FALSE should be "fuse" for equi joins and none for fuzzy joins
  if(isFALSE(keep)) {
    if (fuzzy) return("none")
    return("default")
  }
  if(isTRUE(keep)) return("both")
  keep
}

join_cols <- function(
  x_names, y_names, by = NULL, suffix = c(".x", ".y"), keep = "left",
  # arg from powerjoin
  check, conflicted_cols) {

  #-----------------------------------------------------------------------------
  # original dplyr code
  suffix <- standardise_join_suffix(suffix)
  # x_by and y_by are named vectors of positions
  x_by <- set_names(match(by$x, x_names), by$x)
  y_by <- set_names(match(by$y, y_names), by$x)
  x_loc <- seq_along(x_names)
  y_loc <- seq_along(y_names)
  names(x_loc) <- x_names
  names(y_loc) <- y_names

  if (keep %in% c("left", "default")) {
    # aux cols = not key cols (as named in any table ? not sure why)
    # we exclude by$x here just because we use c(by$x, y_aux) below
    y_aux <- setdiff(y_names, c(by$x, by$y))
    ind <- !x_names %in% c(by$x, conflicted_cols$handled)
    # add suffixes to aux cols in x when they're found in by$x (why would they ?) or in y_aux
    # key cols don't get suffixes
    names(x_loc)[ind] <- add_suffixes(x_names[ind], c(by$x, y_aux), suffix$x)
  } else if (keep %in% c("both", "default_fuzzy")) {
    ind <- !x_names %in% c(conflicted_cols$handled, names(by$equi_keys))
    names(x_loc)[ind] <- add_suffixes(x_names[ind], y_names, suffix$x)
  } else {
    # "right" or "none" : key cols should be taken off x_loc
    ind <- !x_names %in% conflicted_cols$handled
    names(x_loc)[ind] <- add_suffixes(x_names[ind], y_names, suffix$x)
    x_loc <- x_loc[!x_names %in% by$x]
  }

  if(keep == "right") {
    # copy of above with transposed x/y
    x_aux <- setdiff(x_names, c(by$x, by$y))
    ind <- !y_names %in% c(by$y, conflicted_cols$handled)
    names(y_loc)[ind] <- add_suffixes(y_names[ind], c(by$y, x_aux), suffix$y)
  } else if (keep == "both") {
    ind <- !y_names %in% c(conflicted_cols$handled, by$equi_keys)
    names(y_loc)[ind] <- add_suffixes(y_names[ind], x_names, suffix$y)
  } else if (keep == "default_fuzzy") {
    ind <- !y_names %in% c(conflicted_cols$handled, by$equi_keys)
    names(y_loc)[ind] <- add_suffixes(y_names[ind], x_names, suffix$y)
    y_loc <- y_loc[!y_names %in% by$equi_keys]
  } else if (keep == "none") {
    ind <- !y_names %in% conflicted_cols$handled
    names(y_loc)[ind] <- add_suffixes(y_names[ind], setdiff(x_names, by$x), suffix$y)
    y_loc <- y_loc[!y_names %in% by$y]
  } else {
    # "left"  or "default"
    ind <- !y_names %in% conflicted_cols$handled
    names(y_loc)[ind] <- add_suffixes(y_names[ind], x_names, suffix$y)
    y_loc <- y_loc[!y_names %in% by$y]
  }

  list(
    x = list(key = x_by, out = x_loc),
    y = list(key = y_by, out = y_loc),
    conflicted = conflicted_cols$handled)
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
    #---------------------------------------------------------------------------
    # implicit_keys
    check_implicit_keys2(by, check)
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
