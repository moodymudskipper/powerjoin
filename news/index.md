# Changelog

## powerjoin 0.1.0

CRAN release: 2022-11-03

- The join functions will fail if not provided a `check` argument built
  with [`check_specs()`](../reference/check_specs.md)
- New functions [`paste_xy()`](../reference/paste_xy.md) and
  [`paste_yx()`](../reference/paste_xy.md) to conflict columns by
  pasting, ignoring NAs and empty strings by default
- using `rw ~ ...` in the `conflict` argument is better documented
- The class of dates is not lost anymore when using `rw ~ ...` in the
  `conflict` argument
- Obsolete imports were removed
- Rebuilding the documentation solves the issues that got the package
  kicked from CRAN
