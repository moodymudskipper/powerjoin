#' Extended equality operators
#'
#' `%==%` is the bone operator, it works like `==` but `NA %==% 1` is `FALSE` and
#' `NA %==% NA` is `TRUE`. `%in.%` is the a vectorized `%in%`, that
#' can be seen as a rowwise `%in%` when applied to data frame columns. These are
#' convenient helpers for fuzzy joins.
#'
#' @param x A vector
#' @param y A vector for `%==%`, a list of vectors for `%in.%`
#'
#' @export
#' @name extended-equality
#' @examples
#' df1 <- data.frame(key = c("b", "z"))
#' df2 <- data.frame(key1 = c("a", "b", "c"), key2 = c("x", "y", "z"), val = 1:3)
#' power_left_join(df1, df2, ~ .x$key %in.% list(.y$key1, .y$key2))
#'
#' df3 <- data.frame(key1 = c("a", NA))
#' df4 <- data.frame(key2 = c("a", "b", NA), val = 1:3)
#'
#' # note the difference
#' power_inner_join(df3, df4, by = ~ .x$key1 == .y$key2)
#' power_inner_join(df3, df4, by = ~ .x$key1 %==% .y$key2)
#'
#' # typically we would only use the conditions above as part of more complex conditions.
#' # In this precise case they are equivalent to these equi joins
#' power_inner_join(df3, df4, by = c(key1 = "key2"))
#' power_inner_join(df3, df4, by = c(key1 = "key2"), na_matches = "never")
`%==%` <- function(x, y) {
  is.na(x) & is.na(y) | !is.na(x) & !is.na(y) & x == y
}

#' @export
#' @rdname extended-equality
`%in.%` <- function(x, y) {
  conds <- lapply(y, function(yi) x %==% yi)
  Reduce(`|`, conds)
}
