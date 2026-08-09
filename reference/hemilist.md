# Create a hemilist from lh and rh data.

Simply runs `list('lh' = lh_data, 'rh' = rh_data)`: A hemilist (short
for hemisphere list) is just a named list with entries 'lh' and/or 'rh',
which may contain anything. Hemilists are used as parameters and return
values in many `fsbrain` functions. The 'lh' and 'rh' keys typically
contain surfaces or vectors of morphometry data.

## Usage

``` r
hemilist(lh_data = NULL, rh_data = NULL)
```

## Arguments

- lh_data:

  something to wrap, typically some data for a hemisphere, e.g., a
  vector of morphometry data values.

- rh_data:

  something to wrap, typically some data for a hemisphere, e.g., a
  vector of morphometry data values.

## Value

named list, with the 'lh_data' in the 'lh' key and the 'rh_data' in the
'rh' key.

## See also

Other hemilist functions:
[`hemilist.derive.hemi()`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.derive.hemi.md),
[`hemilist.from.prefixed.list()`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.from.prefixed.list.md),
[`hemilist.get.combined.data()`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.get.combined.data.md),
[`hemilist.unwrap()`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.unwrap.md),
[`hemilist.wrap()`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.wrap.md),
[`is.hemilist()`](https://dfsp-spirit.github.io/fsbrain/reference/is.hemilist.md)

## Examples

``` r
  lh_data = rnorm(163842, 5.0, 1.0);
  rh_data = rnorm(163842, 5.0, 1.0);
  hl = hemilist(lh_data, rh_data);
```
