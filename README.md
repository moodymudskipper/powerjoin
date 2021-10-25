
<!-- README.md is generated from README.Rmd. Please edit that file -->

# powerjoin

WIP

{powerjoin} extends {dplyr}’s join functions.

It replaces my {safejoin} package which has un unfortunate homonym on
CRAN and has a suboptimal interface and implementation.

The {dplyr} team is credited since a lot of code is taken from {dplyr}
directly.

We’ll see below how to :

-   Make your joins safer with the `check` argument and the
    `check_specs()`function
-   Preprocess input, for instance to select columns to join without
    having to repeat join columns in the selection
-   Deal with conflicting column names by combining, coalescing them etc
    using the `conflict` argument
-   Do painless unequi-joins thanks to a generalized `by` argument
    accepting formulas
-   fill unmatched values using the `fill` argument
-   operate recursive joins by providing lists of data frames to `x` and
    `y`

## Installation

Install with:

``` r
remotes::install_github("moodymudskipper/powerjoin")
```

## Now let’s match penguins

``` r
library(powerjoin)
library(tidyverse)

# toy dataset built from Allison Horst's {palmerpenguins} package and 
# Hadlew Wickham's {babynames}

male_penguins <- tribble(
     ~name,    ~species,     ~island, ~flipper_length_mm, ~body_mass_g,
 "Giordan",    "Gentoo",    "Biscoe",               222L,        5250L,
  "Lynden",    "Adelie", "Torgersen",               190L,        3900L,
  "Reiner",    "Adelie",     "Dream",               185L,        3650L
)

female_penguins <- tribble(
     ~name,    ~species,  ~island, ~flipper_length_mm, ~body_mass_g,
  "Alonda",    "Gentoo", "Biscoe",               211,        4500L,
     "Ola",    "Adelie",  "Dream",               190,        3600L,
"Mishayla",    "Gentoo", "Biscoe",               215,        4750L,
)
```

# Make your joins safer

The `check` argument receives a list provided by `check_specs()`, whose
arguments can be :

-   `NA` : stay silent (default except for `implicit_keys`)
-   `"inform"`
-   `"warn"`
-   `"abort"`

``` r
check_specs(column_conflict = "abort", duplicate_keys_right = "warn")
#> # powerjoin check specifications
#> ℹ implicit_keys
#> x column_conflict
#> → duplicate_keys_left
#> ! duplicate_keys_right
#> → unmatched_keys_left
#> → unmatched_keys_right
#> → missing_key_combination_left
#> → missing_key_combination_right
#> → inconsistent_factor_levels
#> → inconsistent_type
```

By default it works like {dplyr}, informing in case of implicit keys,
and no further checks :

``` r
power_inner_join(
  male_penguins[2:3], 
  female_penguins[2:3])
#> Joining, by = c("species", "island")
#> # A tibble: 3 × 2
#>   species island
#>   <chr>   <chr> 
#> 1 Gentoo  Biscoe
#> 2 Gentoo  Biscoe
#> 3 Adelie  Dream
```

We can silence the implicit key detection and check that we have unique
keys in the right table

``` r
power_inner_join(
  male_penguins[2:3], 
  female_penguins[2:3],
  check = check_specs(implicit_keys = NA, duplicate_keys_right = "abort"))
#> Error: Keys in the right table have duplicates:
#> # A tibble: 1 × 2
#>   species island
#>   <chr>   <chr> 
#> 1 Gentoo  Biscoe
```

The `column_conflict` argument guarantees that you won’t have columns
renamed without you knowing, you might need it most of the time, we
could setup some development and production specs for our most common
joins:

``` r
dev_specs <- check_specs(
  column_conflict = "abort", 
  inconsistent_factor_levels = "inform",
  inconsistent_type = "inform")

prod_specs <- check_specs(
  column_conflict = "abort", 
  implicit_keys = "abort")
```

This will save some typing :

<!-- For some reason this chunk makes markdown bug, so dirty fix -->

``` r
power_inner_join(
  male_penguins, 
  female_penguins,
  by = c("species", "island"),
  check = dev_specs)
#> Error in `join_cols()` at powerjoin/R/power_join_mutate.R:95:4: 
#> The following columns are ambiguous:  name, flipper_length_mm, body_mass_g
#> Run `rlang::last_error()` to see where the error occurred.
```

## Preprocessing inputs

Traditionally key columns need to be repeated when preprocessing inputs
before a join, which is an annoyance and an opportunity for mistakes.

``` r
inner_join(
  male_penguins %>% select(species, island, name),
  female_penguins %>% select(species, island, female_name = name),
  by = c("species", "island")
)
#> # A tibble: 3 × 4
#>   species island name    female_name
#>   <chr>   <chr>  <chr>   <chr>      
#> 1 Gentoo  Biscoe Giordan Alonda     
#> 2 Gentoo  Biscoe Giordan Mishayla   
#> 3 Adelie  Dream  Reiner  Ola
```

We offer a way around this :

``` r
power_inner_join(
  male_penguins %>% select_keys_and(name),
  female_penguins %>% select_keys_and(female_name = name),
  by = c("species", "island")
)
#> # A tibble: 3 × 4
#>   species island name    female_name
#>   <chr>   <chr>  <chr>   <chr>      
#> 1 Gentoo  Biscoe Giordan Alonda     
#> 2 Gentoo  Biscoe Giordan Mishayla   
#> 3 Adelie  Dream  Reiner  Ola
```

For semi joins, just omit arguments :

``` r
power_inner_join(
  male_penguins,
  female_penguins %>% select_keys_and(),
  by = c("species", "island")
)
#> # A tibble: 3 × 5
#>   name    species island flipper_length_mm body_mass_g
#>   <chr>   <chr>   <chr>              <int>       <int>
#> 1 Giordan Gentoo  Biscoe               222        5250
#> 2 Giordan Gentoo  Biscoe               222        5250
#> 3 Reiner  Adelie  Dream                185        3650
```

We could also aggregate on keys before the join, without the need for
any `group_by()`/`ungroup()` gymnastics :

``` r
power_left_join(
  male_penguins %>% summarize_by_keys(male_weight = mean(body_mass_g)),
  female_penguins %>% summarize_by_keys(female_weight = mean(body_mass_g)),
  by = c("species", "island")
)
#> # A tibble: 3 × 4
#>   species island    male_weight female_weight
#>   <chr>   <chr>           <dbl>         <dbl>
#> 1 Adelie  Dream            3650          3600
#> 2 Adelie  Torgersen        3900            NA
#> 3 Gentoo  Biscoe           5250          4625
```

`pack_along_keys()` packs given columns, or all non key columns by
default, into a data frame column named by the `name` argument, it’s
useful to namespace the data and avoid conflicts

``` r
power_left_join(
  male_penguins %>% pack_along_keys(),
  female_penguins %>% pack_along_keys(),
  by = c("species", "island")
)
#> # A tibble: 4 × 8
#>   species island  name.x  flipper_length_… body_mass_g.x name.y flipper_length_…
#>   <chr>   <chr>   <chr>              <int>         <int> <chr>             <dbl>
#> 1 Gentoo  Biscoe  Giordan              222          5250 Alonda              211
#> 2 Gentoo  Biscoe  Giordan              222          5250 Misha…              215
#> 3 Adelie  Torger… Lynden               190          3900 <NA>                 NA
#> 4 Adelie  Dream   Reiner               185          3650 Ola                 190
#> # … with 1 more variable: body_mass_g.y <int>
```

We have more of these, all variants of tidyverse functions :

-   `nest_by_keys()` nests given columns, or all by default, if `name`
    is given a single list column of data frames is created.
-   `pivot_wider_by_keys()` and `pivot_longer_by_keys()` assume the “id”
    columns are the keys

These functions do not modify the data but add an attribute that will be
processed by the function later on, so no function should be used on top
of them.

## Handle column conflict

To resolve conflicts between identically named join columns, set the
`conflict` argument to a 2 argument function (or formula) that will take
as arguments the 2 conflicting joined columns after the join.

``` r
df1 <- tibble(id = 1:3, value = c(10, NA, 30))
df2 <- tibble(id = 2:4, value = c(22, 32, 42))

power_left_join(df1, df2, by = "id", conflict = `+`)
#> # A tibble: 3 × 2
#>      id value
#>   <int> <dbl>
#> 1     1    NA
#> 2     2    NA
#> 3     3    62
```

coalescing is the most common use case and we have some special values
for it:

``` r
power_left_join(df1, df2, by = "id", conflict = "coalesce_xy")
#> # A tibble: 3 × 2
#>      id value
#>   <int> <dbl>
#> 1     1    10
#> 2     2    22
#> 3     3    30
power_left_join(df1, df2, by = "id", conflict = "coalesce_yx")
#> # A tibble: 3 × 2
#>      id value
#>   <int> <dbl>
#> 1     1    10
#> 2     2    22
#> 3     3    32
```

Note that the function is operating on vectors by default, not row wise,
however we can make it work rowwise by using `rw` in the lhs of the
formula.

``` r
power_left_join(df1, df2, by = "id", conflict = ~ sum(.x, .y, na.rm = TRUE))
#> # A tibble: 3 × 2
#>      id value
#>   <int> <dbl>
#> 1     1    94
#> 2     2    94
#> 3     3    94
power_left_join(df1, df2, by = "id", conflict = rw ~ sum(.x, .y, na.rm = TRUE))
#> # A tibble: 3 × 2
#>      id value
#>   <int> <dbl>
#> 1     1    10
#> 2     2    22
#> 3     3    62
```

If you need finer control, `conflict` can also be a named list of such
functions, formulas or special values, each to be applied on the
relevant pair of conflicted columns.

# fuzzy joins

{powerjoin} builds on David Robinson’s {fuzzyjoin} package’s main
features with the dual goal to simplify the syntax and add to these
operations the other benefits of {powerjoin}.

``` r
power_inner_join(
    male_penguins %>% select_keys_and(name),
    female_penguins %>% select_keys_and(name),
    by = c(~.x$flipper_length_mm < .y$flipper_length_mm & .x$body_mass_g > .y$body_mass_g)
)
#> # A tibble: 1 × 6
#>   flipper_length_mm.x body_mass_g.x name.x flipper_length_… body_mass_g.y name.y
#>                 <int>         <int> <chr>             <dbl>         <int> <chr> 
#> 1                 185          3650 Reiner              190          3600 Ola
```

We might provide different conditions and they’ll be joined with `&`, we
might also mix fuzzy joins with regular equi joins :

``` r
power_inner_join(
    male_penguins %>% select_keys_and(name),
    female_penguins %>% select_keys_and(name),
    by = c("island", ~.x$flipper_length_mm > .y$flipper_length_mm)
)
#> # A tibble: 2 × 5
#>   island flipper_length_mm.x name.x  flipper_length_mm.y name.y  
#>   <chr>                <int> <chr>                 <dbl> <chr>   
#> 1 Biscoe                 222 Giordan                 211 Alonda  
#> 2 Biscoe                 222 Giordan                 215 Mishayla
```

Finally we might want to create a column with a value used in the
comparison, in that case we will use `<-` in the formula (several times
if needed):

``` r
power_inner_join(
    male_penguins %>% select_keys_and(name),
    female_penguins %>% select_keys_and(name),
    by = ~ (mass_ratio <- .y$body_mass_g / .x$body_mass_g) > 1.2
)
#> # A tibble: 3 × 5
#>   body_mass_g.x name.x body_mass_g.y name.y   mass_ratio
#>           <int> <chr>          <int> <chr>         <dbl>
#> 1          3900 Lynden          4750 Mishayla       1.22
#> 2          3650 Reiner          4500 Alonda         1.23
#> 3          3650 Reiner          4750 Mishayla       1.30
```
