#' Coalesce helpers
#'
#' These are wrappers around `dplyr::coalesce`, designed for convenient use in
#' the `conflict` argument of {powerjoin}'s join functions. `coalesce_xy()` is
#' just like `dplyr::coalesce` (except it takes only 2 arguments), `coalesce_yx()`
#' looks first in `y` and then in `x` if `y` is missing.
#'
#' @param x A vector
#' @param y A vector
#'
#' @export
#' @return A vector
#' @examples
#' coalesce_xy(c(NA, 2, 3), c(11, 12, NA))
#' coalesce_yx(c(NA, 2, 3), c(11, 12, NA))
coalesce_xy <- function(x, y) {
  dplyr::coalesce(x, y)
}

#' @rdname coalesce_xy
#' @export
coalesce_yx <- function(x, y) {
  dplyr::coalesce(y, x)
}

#' Paste helpers
#'
#' These are similar to `paste()` but by default ignore `NA` and empty strings
#' (`""`). If they are found in a conflicting column we return the value from
#' the other column without using the separator. If both columns have such values
#' we return an empty string.
#'
#' @param x A vector
#' @param y A vector
#' @param sep separator
#' @param na How to treat `NAs`, they are ignored by default, if `NA` the result
#'   will be `NA`, just as with `stringr::str_c`, if `"NA"` NAs will be coerced
#'   to character just as with `paste()`. Any other string can be used
#' @param ignore_empty Whether to ignore empty strings, to avoid trailing and leading separators
#'
#' @return A character vector
#' @export
#'
#' @examples
#' paste_xy(letters[1:3], c("d", NA, ""))
#' paste_yx(letters[1:3], c("d", NA, ""))
#' paste_xy(letters[1:3], c("d", NA, ""), na = NA, ignore_empty = FALSE)
#' paste_xy(letters[1:3], c("d", NA, ""), na = "NA", ignore_empty = FALSE)
paste_xy <- function(x, y, sep = " ", na = NULL, ignore_empty = TRUE) {
  if (is.null(na) && ignore_empty) {
    x_lgl <- is.na(x) | x == ""
    y_lgl <- is.na(y) | y == ""
    res <- dplyr::case_when(
      x_lgl & y_lgl ~ "",
      x_lgl ~ y,
      y_lgl ~ x,
      TRUE ~ paste(x, y, sep = sep)
    )
    return(res)
  }

  if (is.null(na)) {
    x_lgl <- is.na(x)
    y_lgl <- is.na(y)
    res <- dplyr::case_when(
      x_lgl & y_lgl ~ "",
      x_lgl ~ y,
      y_lgl ~ x,
      TRUE ~ paste(x, y, sep = sep)
    )
    return(res)
  }

  if (is.na(na)) {
    res <- dplyr::case_when(
      is.na(x) | is.na(y) ~ NA_character_,
      TRUE ~ paste(x, y, sep = sep)
    )
    return(res)
  }

  x_lgl <- !is.na(x) & x == ""
  y_lgl <- !is.na(y) & y == ""
  res <- dplyr::case_when(
    x_lgl & y_lgl ~ "",
    x_lgl ~ y,
    y_lgl ~ x,
    TRUE ~ paste(x, y, sep = sep)
  )
  res
}

#' @export
#' @rdname paste_xy
paste_yx <- function(x, y, sep = " ", na = NULL, ignore_empty = TRUE) {
  paste_xy(y, x)
}
