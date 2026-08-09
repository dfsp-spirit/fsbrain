# Create a hemilist from a named list with keys prefixed with 'lh\_' and 'rh\_'.

A hemilist is a named list with entries 'lh' and/or 'rh', see
[`hemilist`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.md).

## Usage

``` r
hemilist.from.prefixed.list(
  named_list,
  report_ignored = TRUE,
  return_ignored = FALSE
)
```

## Arguments

- named_list:

  a named list, the keys must start with 'lh\_' or 'rh\_' to be assigned
  to the 'lh' and 'rh' entries of the returned hemilist. Other entries
  will be ignored.

- report_ignored:

  logical, whether to print a message with the ignored entries, if any.

- return_ignored:

  logical, whether to add a key 'ignored' to the returned hemilist,
  containing the ignored entries.

## Value

a hemilist

## See also

Other hemilist functions:
[`hemilist()`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.md),
[`hemilist.derive.hemi()`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.derive.hemi.md),
[`hemilist.get.combined.data()`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.get.combined.data.md),
[`hemilist.unwrap()`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.unwrap.md),
[`hemilist.wrap()`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.wrap.md),
[`is.hemilist()`](https://dfsp-spirit.github.io/fsbrain/reference/is.hemilist.md)
