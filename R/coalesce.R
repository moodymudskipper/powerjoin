#' Coalesce
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
