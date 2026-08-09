# Unwrap hemi data from a named hemi list.

Unwrap hemi data from a named hemi list.

## Usage

``` r
hemilist.unwrap(hemi_list, hemi = NULL, allow_null_list = FALSE)
```

## Arguments

- hemi_list:

  named list, can have entries 'lh' and/or 'rh', see
  [`hemilist`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.md).

- hemi:

  character string, the hemi data name to retrieve from the list. Can be
  NULL if the list only has a single entry.

- allow_null_list:

  logical, whether to silently return NULL instead of raising an error
  if 'hemi_list' is NULL

## Value

the data

## See also

Other hemilist functions:
[`hemilist()`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.md),
[`hemilist.derive.hemi()`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.derive.hemi.md),
[`hemilist.from.prefixed.list()`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.from.prefixed.list.md),
[`hemilist.get.combined.data()`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.get.combined.data.md),
[`hemilist.wrap()`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.wrap.md),
[`is.hemilist()`](https://dfsp-spirit.github.io/fsbrain/reference/is.hemilist.md)
