# Adapted from join_mutate in dplyr 1.0.7
join_mutate <- function(
  # dplyr args
  x, y, by, copy, type, suffix = c(".x", ".y"), na_matches = c("na", "never"),
  keep = FALSE,
  # powerjoin args
  check = check_specs(),
  conflict = NULL,
  fill = NULL) {
  check <- complete_specs(check)
  #-----------------------------------------------------------------------------
  # implicit_keys
  if(check[["implicit_keys"]] %in% "abort") {
    abort("`by`is `NULL`, join columns should be explicit")
  }
  #-----------------------------------------------------------------------------
  # deal with fuzzy joins
  fml_lgl <- sapply(by, is_formula)
  if(is_formula(by)) {
    fuzzy <- TRUE
    equi_keys <- NULL
    specs <- fuzzy_specs(by)
    by <- specs$multi_by
    multi_match_fun <- specs$multi_match_fun
  } else if(is.list(by) && any(fml_lgl)) {
    fuzzy <- TRUE
    # harmonize unnamed
    names(by) <- allNames(by)
    names(by)[!fml_lgl] <- ifelse(names(by[!fml_lgl]) == "", unlist(by[!fml_lgl]), names(by[!fml_lgl]))
    equi_keys <- by[!fml_lgl]
    # extract lhs
    by[fml_lgl]  <- lapply(by[fml_lgl], `[[`, 2)
    by[!fml_lgl] <- Map(function(x, y) call(
      "==",
      call("$", sym(".x"), sym(x)),
      call("$", sym(".y"), sym(y))),
      names(by[!fml_lgl]), by[!fml_lgl])
    # concat
    by <- Reduce(function(x,y) call("&", x, y), by)
    # rebuild formula
    by <- call("~", by)
    specs <- fuzzy_specs(by)
    by <- specs$multi_by
    multi_match_fun <- specs$multi_match_fun
  } else {
    fuzzy <- FALSE
    #---------------------------------------------------------------------------
    # modified dplyr code
    by <- preprocess_by(tbl_vars(x), tbl_vars(y), by = by, check = check)
  }
  #-----------------------------------------------------------------------------
  # powerjoin preprocess
  x <- preprocess(x, by$x)
  y <- preprocess(y, by$y)
  #-----------------------------------------------------------------------------
  # powerjoin checks
  check_duplicate_keys_left(x, by$x, check)
  check_duplicate_keys_right(y, by$y, check)
  check_missing_key_combination_left(x, by$x, check)
  check_missing_key_combination_right(y, by$y, check)
  check_inconsistent_factor_levels(x, y, by, check)
  check_inconsistent_type(x, y, by, check)
  #-----------------------------------------------------------------------------
  # dplyr original code
  na_equal <- check_na_matches(na_matches)
  x_in <- as_tibble(x, .name_repair = "minimal")
  y_in <- as_tibble(y, .name_repair = "minimal")
  #-----------------------------------------------------------------------------
  # here we should check if we have conflicts handled by the conflict arg
  # if so we take out the conflicted variable(s) before this step
  if(!is.null(conflict)) {
    x_in_vars <- setdiff(names(x), by$x)
    y_in_vars <- setdiff(names(y), by$y)
    # conflicts with by columns can't be fixed, they'll be handled by `column_conflict` check
    conflicted_cols <- intersect(x_in_vars, y_in_vars)
    if(is.list(conflict)) {
      nms <- names(conflict)
      extra_conflicts <- setdiff(nms, conflicted_cols)
      if(length(extra_conflicts)) {
        warn(paste("Some conflict conditions are not used, these columns are not conflicted:",
                   toString(paste0("'", extra_conflicts, "'"))))
        conflicted_cols <- intersect(conflicted_cols, nms)
        conflict = conflict[conflicted_cols]
      }
    }
    conflicted_data <- list(x = x_in[conflicted_cols], y = y_in[conflicted_cols])
    x <- x[!names(x) %in% conflicted_cols]
    y <- y[!names(y) %in% conflicted_cols]
  } else {
    conflicted_data <- NULL
  }
  #-----------------------------------------------------------------------------
  # this doesnt work for fuzzy as it tried to make 1 on 1 matches of by
  # modified dplyr code
  if(fuzzy) {
    vars <- join_cols_fuzzy(tbl_vars(x), tbl_vars(y),
                       by = by, suffix = suffix,
                       keep = keep,
                       # powerjoin args
                       check = check,
                       equi_keys = equi_keys
    )
  } else {
    vars <- join_cols(tbl_vars(x), tbl_vars(y),
                       by = by, suffix = suffix,
                       keep = keep,
                       # powerjoin args
                       check = check
    )
  }

  if(fuzzy) {
    x_key <- x_in[by$x]
    y_key <- y_in[by$y]
    rows <- join_rows_fuzzy(x, y, by, multi_match_fun, mode = type)
  } else {
    x_key <- set_names(x_in[vars$x$key], names(vars$x$key))
    y_key <- set_names(y_in[vars$y$key], names(vars$y$key))
    rows <- join_rows(x_key, y_key, type = type, na_equal = na_equal)
  }
  #-----------------------------------------------------------------------------
  # powerjoin checks
  check_unmatched_keys_left(x, y, by$x, rows, check)
  check_unmatched_keys_right(x, y, by$y, rows, check)
  #-----------------------------------------------------------------------------
  # original dplyr code : rename conflicted and slice
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
  #-----------------------------------------------------------------------------
  # fill
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
  # handle conflicts
  out <- handle_conflicts(out, x_slicer, y_slicer, conflicted_data, conflict)
  #-----------------------------------------------------------------------------
  # add extra columns, this might create a conflict
  if(fuzzy) {
    out <- bind_cols(out, rows$extra_cols)
  }
  #-----------------------------------------------------------------------------
  # original dplyr code
  if (!fuzzy && !keep) {
    key_type <- vec_ptype_common(x_key, y_key)
    out[names(x_key)] <- vec_cast(out[names(x_key)], key_type)
    if (length(rows$y_extra) > 0L) {
      new_rows <- length(rows$x) + seq_along(rows$y_extra)
      out[new_rows, names(y_key)] <- vec_cast(vec_slice(
        y_key,
        rows$y_extra
      ), key_type)

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


# Adapted from join_mutate in dplyr 1.0.7

preprocess_by <- function(x_names, y_names, by = NULL, check) {
  # original dplyr code
  check_duplicate_vars(x_names, "x")
  check_duplicate_vars(y_names, "y")
  by <- standardise_join_by(by, x_names = x_names, y_names = y_names,
                            # arg from powerjoin
                            check = check)
  by
}

join_cols <- function(
  x_names, y_names, by = NULL, suffix = c(".x", ".y"), keep = FALSE,
  # arg from powerjoin
  check) {
  intersect_ <- intersect(x_names, setdiff(y_names, by$y))
  # original dplyr code
  #-----------------------------------------------------------------------------
  #   column_conflict
  if(!is.na(check[["column_conflict"]]) && length(intersect_)) {
    fun <- getFromNamespace(check[["implicit_keys"]], "rlang")
    if(check[["column_conflict"]] == "abort") {
      msg <- paste("The following columns are conflicted: ",
                   toString(paste0("'", intersect_, "'")))
      abort(msg)
    } else {
      msg <- paste(
        "The following columns are conflicted and will be prefixed: ",
        toString(paste0("'", intersect_, "'")))
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

join_cols_fuzzy <- function(
  x_names, y_names, by = NULL, suffix = c(".x", ".y"), keep = FALSE,
  # arg from powerjoin
  check, equi_keys) {
  intersect_ <- intersect(x_names, y_names)
  # original dplyr code
  #-----------------------------------------------------------------------------
  #   column_conflict
  if(!is.na(check[["column_conflict"]]) && length(intersect_)) {
    fun <- getFromNamespace(check[["implicit_keys"]], "rlang")
    if(check[["column_conflict"]] == "abort") {
      msg <- paste("The following columns are conflicted: ",
                   toString(paste0("'", intersect_, "'")))
      abort(msg)
    } else {
      msg <- paste(
        "The following columns are conflicted and will be prefixed: ",
        toString(paste0("'", intersect_, "'")))
      fun(msg)
    }
  }
  #-----------------------------------------------------------------------------
  # original dplyr code
  suffix <- standardise_join_suffix(suffix)
  x_by <- set_names(match(by$x, x_names), by$x)
  y_by <- set_names(match(by$y, y_names), by$y)
  x_loc <- seq_along(x_names)
  names(x_loc) <- x_names
  y_loc <- seq_along(y_names)
  # remove equi keys
  ind <- ! y_names %in% equi_keys
  y_loc <- y_loc[ind]
  y_names <- y_names[ind]


  names(x_loc) <- add_suffixes(x_names, y_names, suffix$x)
  names(y_loc) <- add_suffixes(y_names, x_names, suffix$y)

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
    # implicit_keys
    if(!is.na(check[["implicit_keys"]])) {
      fun <- getFromNamespace(check[["implicit_keys"]], "rlang")
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
