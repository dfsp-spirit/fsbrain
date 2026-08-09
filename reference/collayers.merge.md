# Merge two or more color layers based on their transparency values.

Merge several color layers into one based on their transparency and
alpha blending. In the final result, the lower layers are visible
through the transparent or `NA` parts (if any) of the upper layers.

## Usage

``` r
collayers.merge(collayers, opaque_background = "#FFFFFF")
```

## Arguments

- collayers:

  named list, the values must be vectors, matrices or arrays of color
  strings (as produced by [`rgb`](https://rdrr.io/r/grDevices/rgb.html).
  The names are free form and do not really matter. All values must have
  the same length.

- opaque_background:

  a single color string or `NULL`. If a color string, this color will be
  used as a final opaque background layer to ensure that the returned
  colors are all opaque. Pass `NULL` to skip this, which may result in a
  return value that contains non-opaque color values.

## Value

a color layer, i.e., vector of color strings in a hemilist

## See also

Other surface color layer:
[`collayer.bg()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.bg.md),
[`collayer.bg.atlas()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.bg.atlas.md),
[`collayer.bg.meancurv()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.bg.meancurv.md),
[`collayer.bg.sulc()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.bg.sulc.md),
[`collayer.from.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.from.annot.md),
[`collayer.from.annotdata()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.from.annotdata.md),
[`collayer.from.mask.data()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.from.mask.data.md),
[`collayer.from.morphlike.data()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.from.morphlike.data.md)
