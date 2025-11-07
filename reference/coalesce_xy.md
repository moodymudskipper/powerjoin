# Coalesce helpers

These are wrappers around
[`dplyr::coalesce`](https://dplyr.tidyverse.org/reference/coalesce.html),
designed for convenient use in the `conflict` argument of powerjoin's
join functions. `coalesce_xy()` is just like
[`dplyr::coalesce`](https://dplyr.tidyverse.org/reference/coalesce.html)
(except it takes only 2 arguments), `coalesce_yx()` looks first in `y`
and then in `x` if `y` is missing.

## Usage

``` r
coalesce_xy(x, y)

coalesce_yx(x, y)
```

## Arguments

- x:

  A vector

- y:

  A vector

## Value

A vector

## Examples

``` r
coalesce_xy(c(NA, 2, 3), c(11, 12, NA))
#> [1] 11  2  3
coalesce_yx(c(NA, 2, 3), c(11, 12, NA))
#> [1] 11 12  3
```
