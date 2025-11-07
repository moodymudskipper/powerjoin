# Paste helpers

These are similar to [`paste()`](https://rdrr.io/r/base/paste.html) but
by default ignore `NA` and empty strings (`""`). If they are found in a
conflicting column we return the value from the other column without
using the separator. If both columns have such values we return an empty
string.

## Usage

``` r
paste_xy(x, y, sep = " ", na = NULL, ignore_empty = TRUE)

paste_yx(x, y, sep = " ", na = NULL, ignore_empty = TRUE)
```

## Arguments

- x:

  A vector

- y:

  A vector

- sep:

  separator

- na:

  How to treat `NAs`, they are ignored by default, if `NA` the result
  will be `NA`, just as with
  [`stringr::str_c`](https://stringr.tidyverse.org/reference/str_c.html),
  if `"NA"` NAs will be coerced to character just as with
  [`paste()`](https://rdrr.io/r/base/paste.html). Any other string can
  be used

- ignore_empty:

  Whether to ignore empty strings, to avoid trailing and leading
  separators

## Value

A character vector

## Examples

``` r
paste_xy(letters[1:3], c("d", NA, ""))
#> [1] "a d" "b"   "c"  
paste_yx(letters[1:3], c("d", NA, ""))
#> [1] "d a" "b"   "c"  
paste_xy(letters[1:3], c("d", NA, ""), na = NA, ignore_empty = FALSE)
#> [1] "a d" NA    "c " 
paste_xy(letters[1:3], c("d", NA, ""), na = "NA", ignore_empty = FALSE)
#> [1] "a d"  "b NA" "c"   
```
