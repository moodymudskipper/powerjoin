#' Extended equality operator
#'
#' This is the bone operator, it works like `==` but `NA %==% 1` is `FALSE` and
#' `NA %==% NA` is `TRUE`.
#'
#' @param x A vector
#' @param y A vector
#'
#' @export
`%==%` <- function(x, y) {
  is.na(x) & is.na(y) | !is.na(x) & !is.na(y) & x == y
}
