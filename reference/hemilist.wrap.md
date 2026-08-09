# Wrap data into a named hemi list.

Wrap data into a named hemi list.

## Usage

``` r
hemilist.wrap(data, hemi, hemilist = NULL)
```

## Arguments

- data:

  something to wrap, typically some data for a hemisphere, e.g., a
  vector of morphometry data values. If NULL, the name will not be
  created.

- hemi:

  character string, one of 'lh' or 'rh'. The name to use for the data in
  the returned list.

- hemilist:

  optional
  [`hemilist`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.md),
  an existing hemilist to add the entry to. If left at the default value
  `NULL`, a new list will be created.

## Value

a
[`hemilist`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.md):
a named list, with the 'data' in the name given by parameter 'hemi'

## See also

Other hemilist functions:
[`hemilist()`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.md),
[`hemilist.derive.hemi()`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.derive.hemi.md),
[`hemilist.from.prefixed.list()`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.from.prefixed.list.md),
[`hemilist.get.combined.data()`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.get.combined.data.md),
[`hemilist.unwrap()`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.unwrap.md),
[`is.hemilist()`](https://dfsp-spirit.github.io/fsbrain/reference/is.hemilist.md)
