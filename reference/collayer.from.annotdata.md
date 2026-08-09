# Compute surface color layer from annotation or atlas data.

Compute surface color layer from annotation or atlas data.

## Usage

``` r
collayer.from.annotdata(lh_annotdata = NULL, rh_annotdata = NULL)
```

## Arguments

- lh_annotdata:

  loaded annotation data for left hemi, as returned by
  [`subject.annot`](https://dfsp-spirit.github.io/fsbrain/reference/subject.annot.md)

- rh_annotdata:

  loaded annotation data for right hemi

## Value

named hemi list, each entry is a vector of color strings, one color per
surface vertex. The coloring represents the atlas data.

## See also

You can plot the return value using
[`vis.color.on.subject`](https://dfsp-spirit.github.io/fsbrain/reference/vis.color.on.subject.md).

Other surface color layer:
[`collayer.bg()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.bg.md),
[`collayer.bg.atlas()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.bg.atlas.md),
[`collayer.bg.meancurv()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.bg.meancurv.md),
[`collayer.bg.sulc()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.bg.sulc.md),
[`collayer.from.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.from.annot.md),
[`collayer.from.mask.data()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.from.mask.data.md),
[`collayer.from.morphlike.data()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.from.morphlike.data.md),
[`collayers.merge()`](https://dfsp-spirit.github.io/fsbrain/reference/collayers.merge.md)
