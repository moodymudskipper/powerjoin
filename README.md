
<!-- README.md is generated from README.Rmd. Please edit that file -->

# powerjoin

WIP

{powerjoin} extends {dplyr}’s join functions.

It replaces my {safejoin} package which has un unfortunate homonym on
CRAN and has a suboptimal interface and implementation.

The {dplyr} team is credited since a lot of code is taken from {dplyr}
directly.

## Installation

``` r
remotes::install_github("moodymudskipper/powerjoin")
```

## Example

The `check` argument receives a list provided by `pj_check()`, whose
argument can be :

-   NA : stay silent (default except for `implicit_by`)
-   “inform”
-   “warn”
-   “abort”

``` r
library(powerjoin)
library(dplyr, warn.conflicts = FALSE)

#-------------------------------------------------------------------------------
# works like {dplyr} by default 
power_left_join(
  band_members, 
  band_instruments)
#> Joining, by = "name"
#> # A tibble: 3 × 3
#>   name  band    plays 
#>   <chr> <chr>   <chr> 
#> 1 Mick  Stones  <NA>  
#> 2 John  Beatles guitar
#> 3 Paul  Beatles bass
#-------------------------------------------------------------------------------
# silence the implicit join
power_left_join(
  band_members, 
  band_instruments, 
  check = pj_check(implicit_by = NA))
#> # A tibble: 3 × 3
#>   name  band    plays 
#>   <chr> <chr>   <chr> 
#> 1 Mick  Stones  <NA>  
#> 2 John  Beatles guitar
#> 3 Paul  Beatles bass
#-------------------------------------------------------------------------------
# fail unless explicit `by`
power_left_join(
  band_members, 
  band_instruments, 
  check = pj_check(implicit_by = "abort"))
#> Error: `by`is `NULL`, join columns should be explicit
#-------------------------------------------------------------------------------
# fail if column conflict
power_left_join(
  band_members, 
  band_members,
  by = "name",
  check = pj_check(column_conflict = "abort")
)
#> Error: The following columns are ambiguous:  band
#-------------------------------------------------------------------------------
# fail if duplicate keys in the left table
power_left_join(
  bind_rows(band_members, band_members[1:2,]), 
  band_instruments,
  by = "name",
  check = pj_check(duplicate_keys_left = "abort")
)
#> Error: Keys in the left table have duplicates:
#> # A tibble: 2 × 1
#>   name 
#>   <chr>
#> 1 Mick 
#> 2 John
#-------------------------------------------------------------------------------
# fail if unmatched keys in the left table
power_left_join(
  band_members, 
  band_instruments,
  by = "name",
  check = pj_check(unmatched_keys_left = "abort")
)
#> Error: Keys in the left table have unmatched combinations:
#> # A tibble: 1 × 1
#>   name 
#>   <chr>
#> 1 Mick
#-------------------------------------------------------------------------------
# fail if missing keys combination in the left table
power_left_join(
  band_members, 
  band_members,
  check = pj_check(missing_key_combination_left = "abort")
)
#> Joining, by = c("name", "band")
#> Error: Keys in the left table have missing combinations:
#> # A tibble: 6 × 2
#>   name  band   
#>   <chr> <chr>  
#> 1 Mick  Stones 
#> 2 John  Stones 
#> 3 Paul  Stones 
#> 4 Mick  Beatles
#> 5 John  Beatles
#> 6 Paul  Beatles
#-------------------------------------------------------------------------------
# fail if inconsistent factor levels
power_left_join(
  band_members %>% mutate(name = factor(name)), 
  band_instruments %>% mutate(name = factor(name)), 
  check = pj_check(inconsistent_factor_levels = "abort")
)
#> Joining, by = "name"
#> Error: `name` has different factor levels in the left and right tables
#-------------------------------------------------------------------------------
# fail if inconsistent type
power_left_join(
  band_members %>% mutate(name = factor(name)), 
  band_instruments, 
  check = pj_check(inconsistent_type = "abort")
)
#> Joining, by = "name"
#> Error: `name` has a different type or class in the left and right tables
```
