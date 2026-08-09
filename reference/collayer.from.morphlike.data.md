# Compute surface color layer from morph-like data.

Compute surface color layer from morph-like data.

## Usage

``` r
collayer.from.morphlike.data(
  lh_morph_data = NULL,
  rh_morph_data = NULL,
  makecmap_options = list(colFn = cm.seq()),
  return_metadata = FALSE
)
```

## Arguments

- lh_morph_data:

  numerical vector, can be NULL

- rh_morph_data:

  numerical vector, can be NULL

- makecmap_options:

  named list of parameters to pass to
  [`makecmap`](https://rdrr.io/pkg/squash/man/makecmap.html). Must not
  include the unnamed first parameter, which is derived from 'measure'.

- return_metadata:

  logical, whether to return additional metadata as entry 'metadata' in
  the returned list

## Value

named hemi list, each entry is a vector of color strings, one color per
surface vertex. The coloring represents the morph data.

## See also

You can plot the return value using
[`vis.color.on.subject`](https://dfsp-spirit.github.io/fsbrain/reference/vis.color.on.subject.md).

Other surface color layer:
[`collayer.bg()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.bg.md),
[`collayer.bg.atlas()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.bg.atlas.md),
[`collayer.bg.meancurv()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.bg.meancurv.md),
[`collayer.bg.sulc()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.bg.sulc.md),
[`collayer.from.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.from.annot.md),
[`collayer.from.annotdata()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.from.annotdata.md),
[`collayer.from.mask.data()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.from.mask.data.md),
[`collayers.merge()`](https://dfsp-spirit.github.io/fsbrain/reference/collayers.merge.md)
