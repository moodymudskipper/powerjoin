#' Preprocess powerjoin inputs
#'
#' These functions are named after the tidyverse functions `select`, `summarize`,
#' `nest`, `pack`, `pivot_wider` and `pivot_longer` and are designed to avoid
#' repetition of key columns when preprocessing the data for a join. They should
#' only be used in the `x` and `y` arguments of {powerjoin} join functions. No
#' further transformation should be applied on top of them.
#'
#' Unlike their tidyverse counterparts these just add an attribute to the input and
#' don't reshape it. The join function then preprocesses the inputs using these
#' attributes and the keys.
#'
#' @param name Name of created column
#' @inheritParams tidyr::pivot_wider
#' @inheritParams tidyr::pivot_longer
#' @name preprocess_inputs
#' @return A data frame identical to the `.data` but with a `"powerjoin_preprocess"`
#'   attribute to be handled by the join functions
#' @examples
#' # in practice you'll mostly use those in join function calls directly
#' x <- select_keys_and(head(iris, 2), Sepal.Width)
#' # all it does is add an attribute that will be processed by the join function
#' attr(x, "powerjoin_preprocess")
#' # see `?power_left_join` or README for practical examples
NULL

#' @export
#' @rdname preprocess_inputs
select_keys_and <- function(.data, ...) {
  attr(.data, "powerjoin_preprocess") <- list(type = "select_keys_and", args = enquos(...))
  .data
}

#' @export
#' @rdname preprocess_inputs
summarize_by_keys <- function(.data, ...) {
  attr(.data, "powerjoin_preprocess") <- list(type = "summarize_by_keys", args = enquos(...))
  .data
}

#' @export
#' @rdname preprocess_inputs
nest_by_keys <- function(.data, ..., name = NULL) {
  attr(.data, "powerjoin_preprocess") <- list(type = "nest_by_keys", args = enquos(name = name,...))
  .data
}

#' @export
#' @rdname preprocess_inputs
pack_along_keys <- function(.data, ..., name) {
  if(missing(name)) abort("The `name` argument of `pack_along_keys` cannot be missing")
  attr(.data, "powerjoin_preprocess") <- list(type = "pack_along_keys", args = enquos(name = name, ...))
  .data
}

#' @export
#' @rdname preprocess_inputs
complete_keys <- function(.data) {
  attr(.data, "powerjoin_preprocess") <- list(type = "complete_keys")
  .data
}

# #' @export
# #' @rdname preprocess_inputs
# pivot_wider_by_keys <- function(data, names_from = name, names_prefix = "",
#                                 names_sep = "_", names_glue = NULL, names_sort = FALSE, names_repair = "check_unique",
#                                 values_from = value, values_fill = NULL, values_fn = NULL,
#                                 ...) {
#   attr(data, "powerjoin_preprocess") <- list(type = "pivot_wider_by_keys", args = enquos(
#     names_from = names_from,
#     names_prefix = names_prefix,
#     names_sep = names_sep,
#     names_glue = names_glue,
#     names_sort = names_sort,
#     names_repair = names_repair,
#     values_from = values_from,
#     values_fill = values_fill,
#     values_fn = values_fn,
#     ...))
#   data
# }
#
# #' @export
# #' @rdname preprocess_inputs
# pivot_longer_by_keys <- function(data, names_to = "name", names_prefix = NULL,
#                                  names_sep = NULL, names_pattern = NULL, names_ptypes = list(),
#                                  names_transform = list(), names_repair = "check_unique",
#                                  values_to = "value", values_drop_na = FALSE, values_ptypes = list(),
#                                  values_transform = list(), ...)  {
#   attr(data, "powerjoin_preprocess") <- list(type = "pivot_longer_by_keys", args = enquos(
#     names_to = names_to,
#     names_prefix = names_prefix,
#     names_sep = names_sep,
#     names_pattern = names_pattern,
#     names_ptypes = names_ptypes,
#     names_transform = names_transform,
#     names_repair = names_repair,
#     values_to = values_to,
#     values_drop_na = values_drop_na,
#     values_ptypes = values_ptypes,
#     values_transform = values_transform,
#     ...
#   ))
#   data
# }

preprocess <- function(.data, by) {
  attr_ <- attr(.data, "powerjoin_preprocess")
  if(is.null(attr_)) return(.data)
  attr(.data, 'powerjoin_preprocess') <- NULL

  if(attr_$type == "select_keys_and") {
    # ugly but not sure there's much better
    # if first arg is negative we should start with negative in select too
    # we select `by` as well to be sure they're not removed
    if(length(attr_$args) &&
       is.call(rlang::quo_squash(attr_$args[[1]])) &&
       identical(quo_squash(attr_$args[[1]])[[1]], quote(`-`))) {
      .data <- select(.data, !!!attr_$args, !!by)
    } else {
      .data <- select(.data, !!by, !!!attr_$args)
    }

    return(.data)
  }

  if(attr_$type == "summarize_by_keys") {
    .data <- .data %>%
      group_by_at(by) %>%
      summarize(!!!attr_$args, .groups = "drop")
    return(.data)
  }

  if(attr_$type == "nest_by_keys") {
    name <- eval_tidy(attr_$args$name)
    if(is.null(name)) {
      .data <- .data %>%
        group_by_at(by) %>%
        summarize(across(c(!!!attr_$args[-1]), list), .groups = "drop")
    } else {
      if(length(attr_$args[-1])) {
        .data <- select(.data, !!by, !!!attr_$args[-1])
      }
      .data <- .data %>%
        nest((!!attr_$args$name) := -!!by)
    }
    return(.data)
  }

  # if(attr_$type == "pivot_wider_by_keys") {
  #   # pivot_wider takes default columns name and value, we must not have them so we can give only one
  #   .data <- eval_tidy(expr(pivot_wider(.data, id_cols = !!by, !!!attr_$args)))
  #   return(.data)
  # }
  #
  # if(attr_$type == "pivot_longer_by_keys") {
  #   #
  #   .data <- eval_tidy(expr(pivot_longer(.data, cols = -!!by, !!!attr_$args)))
  #   return(.data)
  # }

  if(attr_$type == "pack_along_keys") {
    .data <- tibble::as_tibble(.data, .name_repair = "minimal")
    pack <- select(.data, -!!by)
    if(length(attr_$args[-1])) {
      pack <- select(pack, !!!attr_$args[-1])
    }
    .data <- transmute(.data, !!!syms(by), (!!attr_$args$name) := pack)
    return(.data)
  }

  if(attr_$type == "complete_keys") {
    cl_bkp <- class(.data)
    .data <- tidyr::complete(.data, !!!syms(by))
    class(.data) <- cl_bkp
    return(.data)
  }
}

