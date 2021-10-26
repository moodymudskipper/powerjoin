
#nocov start

# from dplyr 1.0.7
`glubort` <- function(header, ..., .envir = parent.frame(), .abort = abort) {
  text <- glue(..., .envir = .envir)
  if (!is_null(header)) {
    text <- paste0(header, " ", text)
  }
  .abort(text)
}

# from dplyr 1.0.7
`check_duplicate_vars` <- function(vars, input) {
  dup <- duplicated(vars)
  if (any(dup)) {
    abort(c(glue("Input columns in `{input}` must be unique."),
      x = glue("Problem with {err_vars(vars[dup])}.")
    ))
  }
}

# from dplyr 1.0.7
`bad_args` <- function(args, ..., .envir = parent.frame()) {
  glubort(fmt_args(args), ..., .envir = .envir)
}

# from dplyr 1.0.7
`fmt_args` <- function(x) {
  x <- parse_args(x)
  fmt_obj(x)
}

# from dplyr 1.0.7
`parse_args` <- function(x) {
  x <- unlist(list(x), recursive = FALSE)
  is_fml <- map_lgl(x, is_formula)
  x[is_fml] <- map_chr(map(x[is_fml], "[[", 2), as_string)
  unlist(x)
}

# from dplyr 1.0.7
`fmt_obj` <- function(x) {
  fmt_comma(fmt_obj1(x))
}

# from dplyr 1.0.7
`fmt_comma` <- function(..., .max = 6) {
  x <- paste0(...)
  if (length(x) > .max) {
    length(x) <- .max
    x[[.max]] <- "..."
  }
  commas(x)
}

# from dplyr 1.0.7
`commas` <- function(...) {
  paste0(..., collapse = ", ")
}

# from dplyr 1.0.7
`fmt_obj1` <- function(x) {
  paste0("`", x, "`")
}

# from dplyr 1.0.7
`check_join_vars` <- function(vars, names) {
  if (!is.character(vars)) {
    abort("join columns must be character vectors.")
  }
  na <- is.na(vars)
  if (any(na)) {
    abort(c("Join columns must be not NA.", x = glue("Problem at position {err_vars(na)}.")))
  }
  dup <- duplicated(vars)
  if (any(dup)) {
    abort(c("Join columns must be unique.", x = glue("Problem at position {err_vars(dup)}.")))
  }
  missing <- setdiff(vars, names)
  if (length(missing) > 0) {
    abort(c("Join columns must be present in data.", x = glue("Problem with {err_vars(missing)}.")))
  }
}

# from dplyr 1.0.7
`standardise_join_suffix` <- function(x) {
  if (!is.character(x) || length(x) != 2) {
    abort(c("`suffix` must be a character vector of length 2.",
      i = glue("suffix is {friendly_type_of(x)} of length {length(x)}.")
    ))
  }
  if (any(is.na(x))) {
    bad_args("suffix", "can't be NA.")
  }
  list(x = x[[1]], y = x[[2]])
}

# from dplyr 1.0.7
`tbl_vars` <- function(x) {
  return(new_sel_vars(tbl_vars_dispatch(x), group_vars(x)))
  UseMethod("tbl_vars")
}

# from dplyr 1.0.7
`new_sel_vars` <- function(vars, group_vars) {
  structure(vars, groups = group_vars, class = c(
    "dplyr_sel_vars",
    "character"
  ))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# tbl_vars_dispatch (copied from dplyr:::tbl_vars_dispatch)
# from dplyr 1.0.7
`tbl_vars_dispatch` <- function(x) {
  UseMethod("tbl_vars")
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# tbl_vars.data.frame (copied from dplyr:::tbl_vars.data.frame)
# from dplyr 1.0.7
`tbl_vars.data.frame` <- function(x) {
  names(x)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# check_na_matches (copied from dplyr:::check_na_matches)
# from dplyr 1.0.7
check_na_matches <- function(na_matches = c("na", "never")) {
  if (isNamespaceLoaded("pkgconfig")) {
    conf <- asNamespace("pkgconfig")$get_config("dplyr::na_matches")
    if (!is.null(conf)) {
      warn(c(
        "`dplyr::na_matches` pkgconfig options is now ignored.",
        "Please set `na_matches` directly."
      ))
    }
  }
  arg_match(na_matches) == "na"
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# index_flatten (copied from dplyr:::index_flatten)
# from dplyr 1.0.7
index_flatten <- function(x) {
  unlist(x, recursive = FALSE, use.names = FALSE)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# err_vars (copied from dplyr:::index_flatten)
# from dplyr 1.0.7
err_vars <- function (x)
{
  if (is.logical(x)) {
    x <- which(x)
  }
  if (is.character(x)) {
    x <- encodeString(x, quote = "`")
  }
  glue_collapse(x, sep = ", ", last = if (length(x) <= 2)
    " and "
    else ", and ")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# friendly_type_of (copied from dplyr:::friendly_type_of)
# from dplyr 1.0.7
friendly_type_of <- function (x)
{
  if (is.object(x)) {
    sprintf("a `%s` object", fmt_classes(x))
  }
  else {
    as_friendly_type(typeof(x))
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# fmt_classes (copied from dplyr:::fmt_classes)
# from dplyr 1.0.7
fmt_classes <- function (x)
{
  paste(class(x), collapse = "/")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# add_suffixes (copied from dplyr:::add_suffixes)
# from dplyr 1.0.7
add_suffixes <- function (x, y, suffix)
{
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# as_friendly_type (copied from dplyr:::as_friendly_type)
# from dplyr 1.0.7
as_friendly_type <- function (type)
{
  switch(type, logical = "a logical vector", integer = "an integer vector",
         numeric = , double = "a double vector", complex = "a complex vector",
         character = "a character vector", raw = "a raw vector",
         string = "a string", list = "a list", `NULL` = "NULL",
         environment = "an environment", externalptr = "a pointer",
         weakref = "a weak reference", S4 = "an S4 object", name = ,
         symbol = "a symbol", language = "a call", pairlist = "a pairlist node",
         expression = "an expression vector", quosure = "a quosure",
         formula = "a formula", char = "an internal string", promise = "an internal promise",
         ... = "an internal dots object", any = "an internal `any` object",
         bytecode = "an internal bytecode object", primitive = ,
         builtin = , special = "a primitive function", closure = "a function",
         type)
}

#nocov end

