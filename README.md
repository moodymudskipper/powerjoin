
<!-- README.md is generated from README.Rmd. Please edit that file -->

# powerjoin <img src='man/figures/logo.png' align="right" height="139" />

{powerjoin} extends {dplyr}’s join functions.

-   Make your joins safer with the `check` argument and the
    `check_specs()`function
-   Deal with conflicting column names by combining, coalescing them etc
    using the `conflict` argument
-   Preprocess input, for instance to select columns to join without
    having to repeat key columns in the selection
-   Do painless fuzzy joins thanks to a generalized `by` argument
    accepting formulas
-   Fill unmatched values using the `fill` argument
-   Operate recursive joins by providing lists of data frames to `x` and
    `y`
-   Keep or drop key columns with more flexibility thanks to an enhanced
    `keep`argument

## Installation

Install CRAN version with:

``` r
install.packages("powerjoin")
```

Or development version with:

``` r
remotes::install_github("moodymudskipper/powerjoin")
```

## Now let’s match penguins

``` r
library(powerjoin)
library(tidyverse)

# toy dataset built from Allison Horst's {palmerpenguins} package and 
# Hadley Wickham's {babynames}

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

## Safer joins

The `check` argument receives an object created by the `check_specs()`
function, which provides ways to handle specific input properties, its
arguments can be :

-   `"ignore"` : stay silent (default except for `implicit_keys`)
-   `"inform"`
-   `"warn"`
-   `"abort"`

We can print these defaults :

``` r
check_specs()
#> # powerjoin check specifications
#> ℹ implicit_keys
#> → column_conflict
#> → duplicate_keys_left
#> → duplicate_keys_right
#> → unmatched_keys_left
#> → unmatched_keys_right
#> → missing_key_combination_left
#> → missing_key_combination_right
#> → inconsistent_factor_levels
#> → inconsistent_type
#> → grouped_input
#> → na_keys
```

By default it works like {dplyr}, informing in case of implicit keys,
and no further checks :

``` r
power_inner_join(
  male_penguins[c("species", "island")],
  female_penguins[c("species", "island")]
)
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
check_specs(implicit_keys = "ignore", duplicate_keys_right = "abort")
#> # powerjoin check specifications
#> → implicit_keys
#> → column_conflict
#> → duplicate_keys_left
#> x duplicate_keys_right
#> → unmatched_keys_left
#> → unmatched_keys_right
#> → missing_key_combination_left
#> → missing_key_combination_right
#> → inconsistent_factor_levels
#> → inconsistent_type
#> → grouped_input
#> → na_keys
```

``` r
power_inner_join(
  male_penguins[c("species", "island")],
  female_penguins[c("species", "island")],
  check = check_specs(implicit_keys = "ignore", duplicate_keys_right = "abort")
)
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
  inconsistent_type = "inform"
)

prod_specs <- check_specs(
  column_conflict = "abort",
  implicit_keys = "abort"
)
```

This will save some typing :

<!-- For some reason this chunk makes markdown bug, so dirty fix -->

``` r
power_inner_join(
  male_penguins,
  female_penguins,
  by = c("species", "island"),
  check = dev_specs
)
#> Error: The following columns are conflicted and their conflicts are not handled: 
#> 'name', 'flipper_length_mm', 'body_mass_g'
```

## Handle column conflict

We saw above how to fail when encountering column conflict, here we show
how to handle it.

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

Coalescing is the most common use case and we provide the functions
`coalesce_xy()` and `coalesce_yx()` to ease this task (both wrapped
around `dplyr::coalesce()`).

``` r
power_left_join(df1, df2, by = "id", conflict = coalesce_xy)
#> # A tibble: 3 × 2
#>      id value
#>   <int> <dbl>
#> 1     1    10
#> 2     2    22
#> 3     3    30

power_left_join(df1, df2, by = "id", conflict = coalesce_yx)
#> # A tibble: 3 × 2
#>      id value
#>   <int> <dbl>
#> 1     1    10
#> 2     2    22
#> 3     3    32
```

Note that the function is operating on vectors by default, not rowwise,
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

## Preprocess inputs

Traditionally key columns need to be repeated when preprocessing inputs
before a join, which is an annoyance and an opportunity for mistakes.
With {powerjoin} we can do :

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

For semi joins, just omit arguments to `select_keys_and()`:

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
  male_penguins %>% pack_along_keys(name = "m"),
  female_penguins %>% pack_along_keys(name = "f"),
  by = c("species", "island")
)
#> # A tibble: 4 × 4
#>   species island    m$name $flipper_length… $body_mass_g f$name $flipper_length…
#>   <chr>   <chr>     <chr>             <int>        <int> <chr>             <dbl>
#> 1 Gentoo  Biscoe    Giord…              222         5250 Alonda              211
#> 2 Gentoo  Biscoe    Giord…              222         5250 Misha…              215
#> 3 Adelie  Torgersen Lynden              190         3900 <NA>                 NA
#> 4 Adelie  Dream     Reiner              185         3650 Ola                 190
```

We have more of these, all variants of tidyverse functions :

-   `nest_by_keys()` nests given columns, or all by default, if `name`
    is given a single list column of data frames is created
-   `complete_keys()` expands the key columns, so all combinations are
    present, filling the rest of the new rows with `NA`s. Absent factor
    levels are expanded as well.

<!-- * `pivot_wider_by_keys()` and `pivot_longer_by_keys()` assume the "id" columns are the keys -->

These functions do not modify the data but add an attribute that will be
processed by the join function later on, so no function should be used
on top of them.

## Fuzzy joins

To do fuzzy joins we use formulas in the `by` argument, in this formula
we use, `.x` and `.y` to describe the left and right tables. This is
very flexible but can be costly since a cartesian product is computed.

``` r
power_inner_join(
    male_penguins %>% select_keys_and(male_name = name),
    female_penguins %>% select_keys_and(female_name = name),
    by = c(~.x$flipper_length_mm < .y$flipper_length_mm, ~.x$body_mass_g > .y$body_mass_g)
)
#> # A tibble: 1 × 6
#>   flipper_length_mm.x body_mass_g.x male_name flipper_length_mm.y body_mass_g.y
#>                 <int>         <int> <chr>                   <dbl>         <int>
#> 1                 185          3650 Reiner                    190          3600
#> # … with 1 more variable: female_name <chr>
```

We might also mix fuzzy joins with regular joins :

``` r
power_inner_join(
    male_penguins %>% select_keys_and(male_name = name),
    female_penguins %>% select_keys_and(female_name = name),
    by = c("island", ~.x$flipper_length_mm > .y$flipper_length_mm)
)
#> # A tibble: 2 × 5
#>   island flipper_length_mm.x male_name flipper_length_mm.y female_name
#>   <chr>                <int> <chr>                   <dbl> <chr>      
#> 1 Biscoe                 222 Giordan                   211 Alonda     
#> 2 Biscoe                 222 Giordan                   215 Mishayla
```

Finally we might want to create a column with a value used in the
comparison, in that case we will use `<-` in the formula (several times
if needed)\`:

``` r
power_inner_join(
    male_penguins %>% select_keys_and(male_name = name),
    female_penguins %>% select_keys_and(female_name = name),
    by = ~ (mass_ratio <- .y$body_mass_g / .x$body_mass_g) > 1.2
)
#> # A tibble: 3 × 5
#>   body_mass_g.x male_name body_mass_g.y female_name mass_ratio
#>           <int> <chr>             <int> <chr>            <dbl>
#> 1          3900 Lynden             4750 Mishayla          1.22
#> 2          3650 Reiner             4500 Alonda            1.23
#> 3          3650 Reiner             4750 Mishayla          1.30
```

## Fill unmatched values

The `fill` argument is used to specify what to fill unmatched values
with, note that missing values resulting from matches are not replaced.

``` r
df1 <- tibble(id = 1:3)
df2 <- tibble(id = 1:2, value2 = c(2, NA), value3 = c(NA, 3))

power_left_join(df1, df2, by = "id", fill = 0)
#> # A tibble: 3 × 3
#>      id value2 value3
#>   <int>  <dbl>  <dbl>
#> 1     1      2     NA
#> 2     2     NA      3
#> 3     3      0      0

power_left_join(df1, df2, by = "id", fill = list(value2 = 0))
#> # A tibble: 3 × 3
#>      id value2 value3
#>   <int>  <dbl>  <dbl>
#> 1     1      2     NA
#> 2     2     NA      3
#> 3     3      0     NA
```

## Join recursively

The `x` and `y` arguments accept lists of data frames so one can do :

``` r
df1 <- tibble(id = 1, a = "foo")
df2 <- tibble(id = 1, b = "bar")
df3 <- tibble(id = 1, c = "baz")

power_left_join(list(df1, df2, df3), by = "id")
#> # A tibble: 1 × 4
#>      id a     b     c    
#>   <dbl> <chr> <chr> <chr>
#> 1     1 foo   bar   baz

power_left_join(df1, list(df2, df3), by = "id")
#> # A tibble: 1 × 4
#>      id a     b     c    
#>   <dbl> <chr> <chr> <chr>
#> 1     1 foo   bar   baz
```

## Enhanced `keep` argument

By default, as in *{dplyr}*, key columns are merged and given names from
the left table. In case of a fuzzy join columns that participate in a
fuzzy join are kept from both sides.

We provide additional values `"left"`, `"right"`, `"both"` and `"none"`
to choose which keys to keep or drop.

## Notes

This package supersedes the {safejoin} package which had an unfortunate
homonym on CRAN and had a suboptimal interface and implementation.

Hadley Wickham, Romain François and David Robinson are credited for
their work in {dplyr} and {fuzzyjoin} since this package contains some
code copied from these packages.
