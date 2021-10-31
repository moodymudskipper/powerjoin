fuzzy_specs <- function(by) {

  by_x <- list()
  by_y <- list()
  new_cols <- list()
  rec <- function(call) {
    if(!is.call(call) || length(call) == 1) return(call)
    calling_dollar <- identical(call[[1]], quote(`$`))
    calling_dollar_on_x <- calling_dollar && identical(call[[2]], quote(`.x`))
    if(calling_dollar_on_x) {
      var <- as.character(call[[3]])
      by_x[[length(by_x) + 1]] <<- var
      return(call)
    }
    calling_dollar_on_y <- calling_dollar && identical(call[[2]], quote(`.y`))
    if(calling_dollar_on_y) {
      var <- as.character(call[[3]])
      by_y[[length(by_y) + 1]] <<- var
      return(call)
    }
    calling_arrow <- identical(call[[1]], quote(`<-`))
    if(calling_arrow) {
      new_cols[[length(new_cols) + 1]] <<- as.character(call[[2]])
    }
    call[] <- lapply(call, rec)
    call
  }
  multi_match_fun_body <- rec(by[[2]])
  by_x <- unlist(unique(by_x))
  by_y <- unlist(unique(by_y))
  if(length(new_cols)) {
    multi_match_fun_body <- as.call(c(quote(data.frame), ..match.. = multi_match_fun_body, syms(new_cols)))
  }

  #multi_match_fun_body <-  # depends if we have new cols
  multi_match_fun <- as.function(c(alist(.x=,.y=), multi_match_fun_body))
  multi_by <- list(x = by_x, y = by_y)
  list(
    multi_match_fun = multi_match_fun,
    multi_by = multi_by
  )
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

join_rows_fuzzy <- function(x, y, by, multi_match_fun, mode = "left") {
  multi_match_fun <- purrr::as_mapper(multi_match_fun)

  # use multiple matches
  # by <- common_by(multi_by, x, y)

  number_x_rows <- nrow(x)
  number_y_rows <- nrow(y)

  indices_x <- x %>%
    select_at(by$x) %>%
    mutate(indices = seq_len(number_x_rows)) %>%
    group_by_at(vars(-one_of("indices"))) %>%
    tidyr::nest() %>%
    mutate(indices = purrr::map(data, "indices")) %>%
    ungroup()

  indices_y <- y %>%
    select_at(by$y) %>%
    mutate(indices = seq_len(number_y_rows)) %>%
    group_by_at(vars(-one_of("indices"))) %>%
    tidyr::nest() %>%
    mutate(indices = purrr::map(data, "indices")) %>%
    ungroup()

  ux <- indices_x[by$x]
  uy <- indices_y[by$y]

  ix <- rep(seq(nrow(ux)), nrow(uy))
  iy <- rep(seq(nrow(uy)), each = nrow(ux))

  ux_input <- slice(ux, ix)
  uy_input <- slice(uy, iy)

  m <- multi_match_fun(ux_input, uy_input)

  extra_cols <- NULL
  if (is.data.frame(m)) {
    if (ncol(m) > 1) {
      extra_cols <- m[, -1, drop = FALSE]
    }
    m <- m[[1]]
  }

  if (sum(m) == 0) {
    # there are no matches
    matches <- tibble::tibble(x = numeric(0), y = numeric(0))
    matches <- bind_cols(matches, extra_cols[0,, drop = FALSE])
  } else {

    x_indices_l <- indices_x$indices[ix[m]]
    y_indices_l <- indices_y$indices[iy[m]]
    xls <- lengths(x_indices_l)
    yls <- lengths(y_indices_l)
    x_rep <- unlist(purrr::map2(x_indices_l, yls, function(x, y) rep(x, each = y)))
    y_rep <- unlist(purrr::map2(y_indices_l, xls, function(y, x) rep(y, x)))

    matches <- tibble::tibble(x = x_rep, y = y_rep)

    if (!is.null(extra_cols)) {
      extra_indices <- rep(which(m), xls * yls)
      extra_cols_rep <- extra_cols[extra_indices, , drop = FALSE]
      matches <- bind_cols(matches, extra_cols_rep)
    }
  }

  #-----------------------------------------------------------------------------
  matches <- arrange(matches, x, y)

  # fill in indices of the x, y, or both
  # curious if there's a higher performance approach
  if (mode == "left") {
    matches <- tibble::tibble(x = seq_len(number_x_rows)) %>%
      left_join(matches, by = "x")
  } else if (mode == "right") {
    matches <- tibble::tibble(y = seq_len(number_y_rows)) %>%
      left_join(matches, by = "y")
  } else if (mode == "full") {
    matches <- matches %>%
      full_join(tibble::tibble(x = seq_len(number_x_rows)), by = "x") %>%
      full_join(tibble::tibble(y = seq_len(number_y_rows)), by = "y")
  }

  #-----------------------------------------------------------------------------
  # harmonize with dplyr
  na_x_ind <- is.na(matches$x)
  y_extra <- matches$y[na_x_ind]
  x_loc <- matches$x[!na_x_ind]
  y_loc <- matches$y[!na_x_ind]
  x_unmatched <- setdiff(seq(number_x_rows), x_loc[!is.na(y_loc)])
  y_unmatched <- setdiff(seq(number_y_rows), y_loc[!is.na(x_loc)])

  extra_cols <- matches[-(1:2)]

  list(x = x_loc, y = y_loc, y_extra = y_extra,
       x_unmatched = x_unmatched, y_unmatched = y_unmatched,
       extra_cols = extra_cols)
}



