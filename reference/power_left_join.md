# Power joins

Power joins

## Usage

``` r
power_left_join(
  x,
  y = NULL,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  keep = NULL,
  na_matches = c("na", "never"),
  check = check_specs(),
  conflict = NULL,
  fill = NULL
)

power_right_join(
  x,
  y = NULL,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  keep = NULL,
  na_matches = c("na", "never"),
  check = check_specs(),
  conflict = NULL,
  fill = NULL
)

power_inner_join(
  x,
  y = NULL,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  keep = NULL,
  na_matches = c("na", "never"),
  check = check_specs(),
  conflict = NULL,
  fill = NULL
)

power_full_join(
  x,
  y = NULL,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  keep = NULL,
  na_matches = c("na", "never"),
  check = check_specs(),
  conflict = NULL,
  fill = NULL
)
```

## Arguments

- x, y:

  A pair of data frames, data frame extensions (e.g. a tibble), or lazy
  data frames (e.g. from dbplyr or dtplyr). See *Methods*, below, for
  more details.

- by:

  As in dplyr, but extended so user can supply a formula or a list of
  character and formulas. Formulas are used for fuzzy joins, see
  dedicated section below.

- copy:

  Ignored at the moment because powerjoin doesn't support databases

- suffix:

  If there are non-joined duplicate variables in `x` and `y`, these
  suffixes will be added to the output to disambiguate them. Should be a
  character vector of length 2.

- keep:

  A boolean for compatibility with dplyr, or a value among "left",
  "right", "both", "none" or "default". See dedicated section below.

- na_matches:

  Should two `NA` or two `NaN` values match?

  - `"na"`, the default, treats two `NA` or two `NaN` values as equal,
    like `%in%`, [`match()`](https://rdrr.io/r/base/match.html), and
    [`merge()`](https://rdrr.io/r/base/merge.html).

  - `"never"` treats two `NA` or two `NaN` values as different, and will
    never match them together or to any other values. This is similar to
    joins for database sources and to `base::merge(incomparables = NA)`.

- check:

  A list created with [`check_specs()`](check_specs.md)

- conflict:

  A function, formula, the special value amongst `"patch"`, or a named
  list of such items. If the LHS of the formula is `rw` the rhs will be
  applied rowwise. Note that the columns will be subset with `[` so for
  list columns `.x` or `.y` will refer to length 1 lists and you might
  sometimes need `.x[[1]]` or `.y[[1]]`.

- fill:

  Values used to replace missing values originating in unmatched keys,
  or a named list of such items.

## Value

A data frame

## `keep` argument values

- `NULL` (default) : merge keys and name them as the left table's keys,
  and keep columns used for fuzzy joins from both tables

- `left` : keep only key columns for left table

- `right`: keep only key columns for right table

- `both` or `TRUE`: keep key columns from both tables, adding suffix if
  relevant

- `none` : drop all key columns from the output

- `FALSE` : merge keys and name them as the left table's keys, maps to
  `none` for fuzzy joins

## fuzzy joins

To specify fuzzy matching conditions we use formulas in which we refer
to the columns from the left side data frame using `.x` and the right
side data frame using `.y`, for instance `by = ~ .x$col1 > .y$col2`.

We can specify several conditions and even mix equi condition with fuzzy
condition, for instance `by = c(col1 = "col2", ~ .x$col3 > .y$col4)`

To fuzzy match strings we can leverage the functions from the stringr
package since they are vectorized on all main arguments, for instance to
match observations where `col1` contains `col1` we can attach stringr
and do `by = ~ str_detect(.x$col1, fixed(.y$col2))`.

Another useful function is `stringdist` from the stringdist package to
match strings that are close enough, for instance
`by = ~ stringdist::stringdist(.x$a,.y$a) < 2`

We can also define a new column computed during the fuzzy matching,
using the arrow assignment operator, for instance :
`by = ~ .x$col1 > (mysum <- .y$col2 + .y$col3)`

When the `by` condition evaluates to `NA` the observation is dismissed.
This makes `by = c(a = "b")` slightly different from
`by = ~ .x$a == .y$b` when `na_matches` is `"na"` (the default). To be
able to match `NA` with `NA` in fuzzy matching condition we can use the
`%==%` operator (bone operator), defined in this package.

## Examples

``` r
# See README for a more verbose version
library(tibble)
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

# apply different checks
power_inner_join(
  male_penguins[c("species", "island")],
  female_penguins[c("species", "island")],
  check = check_specs(implicit_keys = "ignore", duplicate_keys_right = "inform")
)
#> Keys in the right table have duplicates:
#> # A tibble: 1 × 2
#>   species island
#>   <chr>   <chr> 
#> 1 Gentoo  Biscoe
#> # A tibble: 3 × 2
#>   species island
#>   <chr>   <chr> 
#> 1 Gentoo  Biscoe
#> 2 Gentoo  Biscoe
#> 3 Adelie  Dream 

df1 <- tibble(id = 1:3, value = c(10, NA, 30))
df2 <- tibble(id = 2:4, value = c(22, 32, 42))

# handle conflicted columns when joining
power_left_join(df1, df2, by = "id", conflict = `+`)
#> # A tibble: 3 × 2
#>      id value
#>   <int> <dbl>
#> 1     1    NA
#> 2     2    NA
#> 3     3    62

# the most frequent use case is to coalesce
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

# the conflict function is applied colwise by default!
power_left_join(df1, df2, by = "id", conflict = ~ sum(.x, .y, na.rm = TRUE))
#> # A tibble: 3 × 2
#>      id value
#>   <int> <dbl>
#> 1     1    94
#> 2     2    94
#> 3     3    94

# apply conflict function rowwise
power_left_join(df1, df2, by = "id", conflict = rw ~ sum(.x, .y, na.rm = TRUE))
#> # A tibble: 3 × 2
#>      id value
#>   <int> <dbl>
#> 1     1    10
#> 2     2    22
#> 3     3    62

# subset columns without repeating keys
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

# semi join
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

# agregate without repeating keys
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

# pack auxiliary colums without repeating keys
power_left_join(
  male_penguins %>% pack_along_keys(name = "m"),
  female_penguins %>% pack_along_keys(name = "f"),
  by = c("species", "island")
)
#> # A tibble: 4 × 4
#>   species island    m$name  $flipper_length_mm $body_mass_g f$name  
#>   <chr>   <chr>     <chr>                <int>        <int> <chr>   
#> 1 Gentoo  Biscoe    Giordan                222         5250 Alonda  
#> 2 Gentoo  Biscoe    Giordan                222         5250 Mishayla
#> 3 Adelie  Torgersen Lynden                 190         3900 NA      
#> 4 Adelie  Dream     Reiner                 185         3650 Ola     
#> # ℹ 2 more variables: f$flipper_length_mm <dbl>, $body_mass_g <int>

# fuzzy join
power_inner_join(
  male_penguins %>% select_keys_and(male_name = name),
  female_penguins %>% select_keys_and(female_name = name),
  by = c(~.x$flipper_length_mm < .y$flipper_length_mm, ~.x$body_mass_g > .y$body_mass_g)
)
#> # A tibble: 1 × 6
#>   flipper_length_mm.x body_mass_g.x male_name flipper_length_mm.y body_mass_g.y
#>                 <int>         <int> <chr>                   <dbl>         <int>
#> 1                 185          3650 Reiner                    190          3600
#> # ℹ 1 more variable: female_name <chr>

# fuzzy + equi join
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

# define new column without repeating computation
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
power_inner_join(
  male_penguins %>% select_keys_and(male_name = name),
  female_penguins %>% select_keys_and(female_name = name),
  by = ~ (mass_ratio <- .y$body_mass_g / .x$body_mass_g) > 1.2,
  keep = "none"
)
#> # A tibble: 3 × 3
#>   male_name female_name mass_ratio
#>   <chr>     <chr>            <dbl>
#> 1 Lynden    Mishayla          1.22
#> 2 Reiner    Alonda            1.23
#> 3 Reiner    Mishayla          1.30

# fill unmatched values
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

# join recursively
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
