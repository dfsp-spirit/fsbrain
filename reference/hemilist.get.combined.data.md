# Get combined data of hemi list

Get combined data of hemi list

## Usage

``` r
hemilist.get.combined.data(hemi_list)
```

## Arguments

- hemi_list:

  named list, can have entries 'lh' and/or 'rh', see
  [`hemilist`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.md)

## Value

the data combined with [`c`](https://rdrr.io/r/base/c.html), or NULL if
both entries are NULL.

## See also

Other hemilist functions:
[`hemilist()`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.md),
[`hemilist.derive.hemi()`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.derive.hemi.md),
[`hemilist.from.prefixed.list()`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.from.prefixed.list.md),
[`hemilist.unwrap()`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.unwrap.md),
[`hemilist.wrap()`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.wrap.md),
[`is.hemilist()`](https://dfsp-spirit.github.io/fsbrain/reference/is.hemilist.md)
