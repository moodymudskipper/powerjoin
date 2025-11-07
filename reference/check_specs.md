# Build a checklist for power joins

Build a checklist for power joins

## Usage

``` r
check_specs(
  implicit_keys = c("inform", "ignore", "warn", "abort"),
  column_conflict = c("ignore", "inform", "warn", "abort"),
  duplicate_keys_left = c("ignore", "inform", "warn", "abort"),
  duplicate_keys_right = c("ignore", "inform", "warn", "abort"),
  unmatched_keys_left = c("ignore", "inform", "warn", "abort"),
  unmatched_keys_right = c("ignore", "inform", "warn", "abort"),
  missing_key_combination_left = c("ignore", "inform", "warn", "abort"),
  missing_key_combination_right = c("ignore", "inform", "warn", "abort"),
  inconsistent_factor_levels = c("ignore", "inform", "warn", "abort"),
  inconsistent_type = c("ignore", "inform", "warn", "abort"),
  grouped_input = c("ignore", "inform", "warn", "abort"),
  na_keys = c("ignore", "inform", "warn", "abort")
)
```

## Arguments

- implicit_keys:

  What to do if keys are not given explicitly through the `by` argument

- column_conflict:

  What to do if the join creates a column conflict which is not handled
  by the `conflict` argument

- duplicate_keys_left:

  What to do if we find duplicate sets of keys in the left table

- duplicate_keys_right:

  What to do if we find duplicate sets of keys in the right table

- unmatched_keys_left:

  What to do if we find unmatched sets of keys in the left table

- unmatched_keys_right:

  What to do if we find unmatched sets of keys in the right table

- missing_key_combination_left:

  What to do if the left table doesn't contain all key combinations

- missing_key_combination_right:

  What to do if the right table doesn't contain all key combinations

- inconsistent_factor_levels:

  What to do if the key columns from both sides have inconsistent factor
  levels

- inconsistent_type:

  What to do if the joined keys have a different type

- grouped_input:

  What to do if one or both of the tables are grouped

- na_keys:

  What to do if keys contain missing values

## Value

A character vector of class `"powerjoin_check"`

## Examples

``` r
check_specs(
  implicit_keys = "ignore",
  grouped_input = "inform",
  column_conflict = "abort",
  na_keys ="warn")
#> # powerjoin check specifications
#> → implicit_keys
#> ✖ column_conflict
#> → duplicate_keys_left
#> → duplicate_keys_right
#> → unmatched_keys_left
#> → unmatched_keys_right
#> → missing_key_combination_left
#> → missing_key_combination_right
#> → inconsistent_factor_levels
#> → inconsistent_type
#> ℹ grouped_input
#> ! na_keys
```
