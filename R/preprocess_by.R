preprocess_by <- function(x_names, y_names, by = NULL, check) {
  fml_lgl <- sapply(by, is_formula)
  if(is_formula(by)) {
    equi_keys <- NULL
    specs <- fuzzy_specs(by)
    by <- specs$multi_by
    by$multi_match_fun <- specs$multi_match_fun
    by$extra_cols <- specs$extra_cols
    by$fuzzy <- TRUE
  } else if(is.list(by) && any(fml_lgl)) {
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
    by$multi_match_fun <- specs$multi_match_fun
    by$fuzzy <- TRUE
    by$equi_keys <- equi_keys
    by$extra_cols <- specs$extra_cols
  } else {
    fuzzy <- FALSE
    #---------------------------------------------------------------------------
    # modified dplyr code
    by <- preprocess_by_equi(x_names, y_names, by = by, check = check)
    by$fuzzy <- FALSE
  }
  by
}

# Adapted from join_mutate in dplyr 1.0.7
preprocess_by_equi <- function(x_names, y_names, by = NULL, check) {
  # original dplyr code
  check_duplicate_vars(x_names, "x")
  check_duplicate_vars(y_names, "y")
  by <- standardise_join_by(by, x_names = x_names, y_names = y_names,
                            # arg from powerjoin
                            check = check)
  by
}
