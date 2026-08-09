# Create labeldata from a mask.

Create labeldata from a mask. This function is trivial and only calls
[`which`](https://rdrr.io/r/base/which.html) after performing basic
sanity checks.

## Usage

``` r
labeldata.from.mask(mask, invert = FALSE)
```

## Arguments

- mask:

  a logical vector

- invert:

  Whether to report the inverse the mask before determining the indices.
  Defaults to FALSE.

## Value

labeldata. The list of indices which are TRUE in the mask (or the ones
which FALSE if 'invert' is TRUE).

## See also

Other label data functions:
[`group.label()`](https://dfsp-spirit.github.io/fsbrain/reference/group.label.md),
[`mask.from.labeldata.for.hemi()`](https://dfsp-spirit.github.io/fsbrain/reference/mask.from.labeldata.for.hemi.md),
[`subject.label()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.label.md)
