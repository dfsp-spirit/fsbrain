# Match per-bundle values to the bundles.

Match per-bundle values to the bundles.

## Usage

``` r
match.bundle.values(bundle_values, bundles)
```

## Arguments

- bundle_values:

  named or unnamed numeric vector, or NULL.

- bundles:

  named list of `fs.tracts` instances, see
  [`read.tract.bundles`](https://dfsp-spirit.github.io/fsbrain/reference/read.tract.bundles.md).

## Value

numeric vector of length `length(bundles)`, or NULL if `bundle_values`
was NULL. The values are in the order of `bundles`.
