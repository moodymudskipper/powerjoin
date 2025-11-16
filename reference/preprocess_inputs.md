# Preprocess powerjoin inputs

These functions are named after the tidyverse (dplyr and tidyr)
functions `select`, `summarize`, `nest`, `pack`, `pivot_wider` and
`pivot_longer` and are designed to avoid repetition of key columns when
preprocessing the data for a join. They should only be used in the `x`
and `y` arguments of powerjoin join functions. No further transformation
should be applied on top of them.

## Usage

``` r
select_keys_and(.data, ...)

summarize_by_keys(.data, ...)

nest_by_keys(.data, ..., name = NULL)

pack_along_keys(.data, ..., name)

complete_keys(.data)
```

## Arguments

- .data:

  A data frame to pivot.

- ...:

  Additional arguments passed on to methods.

- name:

  Name of created column

## Value

A data frame identical to the `.data` but with a
`"powerjoin_preprocess"` attribute to be handled by the join functions

## Details

Unlike their tidyverse counterparts these just add an attribute to the
input and don't reshape it. The join function then preprocesses the
inputs using these attributes and the keys.

## Examples

``` r
# in practice you'll mostly use those in join function calls directly
x <- select_keys_and(head(iris, 2), Sepal.Width)
# all it does is add an attribute that will be processed by the join function
attr(x, "powerjoin_preprocess")
#> $type
#> [1] "select_keys_and"
#> 
#> $args
#> <list_of<quosure>>
#> 
#> [[1]]
#> <quosure>
#> expr: ^Sepal.Width
#> env:  0x556adf5a5398
#> 
#> 
# see `?power_left_join` or README for practical examples
```
