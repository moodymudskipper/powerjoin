# powerjoin 0.2.0

## New features

* New operators `%==%` and `%in.%` (#46). `%==%` works like `==` but treats `NA`
  as a regular value (`NA %==% NA` is `TRUE`, `NA %==% 1` is `FALSE`), and `%in.%`
  is a vectorized `%in%`. Both are convenient in fuzzy join conditions.
* Fuzzy join conditions support `[[`, e.g. `.x[["col"]]` (#37).

## Bug fixes

* Fuzzy joins mixing equi and non-equi conditions now give correct results (#42).
* Fuzzy join conditions that evaluate to `NA` are now treated as non matches (#41).
* `check_specs(unmatched_keys_left = )` and `check_specs(unmatched_keys_right = )`
  don't fail anymore on data frames joined by a single key (#85).

## Maintenance

* Rd files use `\pkg{}` instead of plain braces, fixing the "Lost braces" NOTE.
* Compatibility with recent versions of dplyr and vctrs.
* Typo fixes in the documentation (@luisDVA, #51).

# powerjoin 0.1.0

* The join functions will fail if not provided a `check` argument built with `check_specs()`
* New functions `paste_xy()` and `paste_yx()` to conflict columns by pasting, ignoring NAs and 
  empty strings by default
* using `rw ~ ...` in the `conflict` argument is better documented
* The class of dates is not lost anymore when using `rw ~ ...` in the `conflict` argument
* Obsolete imports were removed
* Rebuilding the documentation solves the issues that got the package kicked from CRAN
