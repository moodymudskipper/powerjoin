
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

## Safe joins

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

## Preprocessing inputs

Traditionally key columns need to be repeated when preprocessing inputs
before a join, we offer a way around this.

compare the following versions of {dplyr} joins vs {powerjoin}

``` r
library(dplyr, warn.conflicts = FALSE)

#-------------------------------------------------------------------------------
x <- tibble(Species = "setosa", Sepal.Length = 5.1)

# with {dplyr}, select key columns + columns to join
left_join(
  x,
  iris %>% select(Species, Sepal.Length, Petal.Length)
)
#> Joining, by = c("Species", "Sepal.Length")
#> # A tibble: 8 × 3
#>   Species Sepal.Length Petal.Length
#>   <chr>          <dbl>        <dbl>
#> 1 setosa           5.1          1.4
#> 2 setosa           5.1          1.4
#> 3 setosa           5.1          1.5
#> 4 setosa           5.1          1.5
#> 5 setosa           5.1          1.7
#> 6 setosa           5.1          1.5
#> 7 setosa           5.1          1.9
#> 8 setosa           5.1          1.6
#-------------------------------------------------------------------------------
# with {powerjoin}, only select columns to join
power_left_join(
  x,
  iris %>% select_keys_and(Petal.Length)
)
#> Joining, by = c("Species", "Sepal.Length")
#> # A tibble: 8 × 3
#>   Species Sepal.Length Petal.Length
#>   <chr>          <dbl>        <dbl>
#> 1 setosa           5.1          1.4
#> 2 setosa           5.1          1.4
#> 3 setosa           5.1          1.5
#> 4 setosa           5.1          1.5
#> 5 setosa           5.1          1.7
#> 6 setosa           5.1          1.5
#> 7 setosa           5.1          1.9
#> 8 setosa           5.1          1.6
#-------------------------------------------------------------------------------
x <- tibble(Species = c("setosa", "virginica"))

# with {dplyr}, group by key columns first then summarize
left_join(
  x,
  iris %>% group_by(Species) %>% summarize(Sepal.Length = mean(Sepal.Length), .groups = "drop")
)
#> Joining, by = "Species"
#> # A tibble: 2 × 2
#>   Species   Sepal.Length
#>   <chr>            <dbl>
#> 1 setosa            5.01
#> 2 virginica         6.59
#-------------------------------------------------------------------------------
# with {powerjoin}, summrize by key columns right away 
power_left_join(
  x,
  iris %>% summarize_by_keys(Sepal.Length = mean(Sepal.Length))
)
#> Joining, by = "Species"
#> # A tibble: 2 × 2
#>   Species   Sepal.Length
#>   <chr>            <dbl>
#> 1 setosa            5.01
#> 2 virginica         6.59
```

We have more of these, all variants of tidyverse functions :

-   `nest_by_keys()` nests given columns, or all by default, if `name`
    is given a sinle list column of data frames is created.
-   `pack_along_keys()` packs given columns, or all non key columns by
    default, into a a data frame column named by the `name` argument,
    it’s useful to namespace the data and avoid conflicts
-   `pivot_wider_by_keys()` and `pivot_longer_by_keys()` assume the “id”
    columns are the keys

## Handle column conflict

To resolve conflicts between identically named join columns, set the
`conflict` argument to a 2 argument function that will take as arguments
the 2 conflicting joined columns after the join.

coalescing is the most common use case but the feature is flexible

``` r
df1 <- tibble(id = 1:3, value = c(10, NA, 30))
df2 <- tibble(id = 2:4, value = c(22, 32, 42))

#-------------------------------------------------------------------------------
# no conflict handling
power_left_join(df1, df2, by = "id")
#> # A tibble: 3 × 3
#>      id value.x value.y
#>   <int>   <dbl>   <dbl>
#> 1     1      10      NA
#> 2     2      NA      22
#> 3     3      30      32
#-------------------------------------------------------------------------------
# coalescing conflicting cols
power_left_join(df1, df2, by = "id", conflict = dplyr::coalesce)
#> # A tibble: 3 × 2
#>      id value
#>   <int> <dbl>
#> 1     1    10
#> 2     2    22
#> 3     3    30
#-------------------------------------------------------------------------------
# we are operating on vectors by default, not row wise!
power_left_join(df1, df2, by = "id", conflict = ~ sum(.x, .y, na.rm = TRUE))
#> # A tibble: 3 × 2
#>      id value
#>   <int> <dbl>
#> 1     1    94
#> 2     2    94
#> 3     3    94
#-------------------------------------------------------------------------------
# for row wise operations, type `rw` on the left side of the formula
power_left_join(df1, df2, by = "id", conflict = rw ~ sum(.x, .y, na.rm = TRUE))
#> # A tibble: 3 × 2
#>      id value
#>   <int> <dbl>
#> 1     1    10
#> 2     2    22
#> 3     3    62
```
