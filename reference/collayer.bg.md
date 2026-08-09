# Compute binarized mean curvature surface color layer.

Compute a binarized mean curvature surface color layer, this is intended
as a background color layer. You can merge it with your data layer using
[`collayers.merge`](https://dfsp-spirit.github.io/fsbrain/reference/collayers.merge.md).

## Usage

``` r
collayer.bg(subjects_dir, subject_id, bg, hemi = "both")
```

## Arguments

- subjects_dir:

  character string, the FreeSurfer SUBJECTS_DIR.

- subject_id:

  character string, the subject identifier.

- bg:

  character string, a background name. One of 'curv', 'curv_light',
  'sulc', 'sulc_light', or 'aparc'. If this is already a colorlayer in a
  hemilist, it will be returned as-is.

- hemi:

  character string, one of 'lh', 'rh', or 'both'. The latter will merge
  the data for both hemis into a single vector.

## Value

a color layer, i.e., vector of color strings in a hemilist

## See also

You can plot the return value using
[`vis.color.on.subject`](https://dfsp-spirit.github.io/fsbrain/reference/vis.color.on.subject.md).

Other surface color layer:
[`collayer.bg.atlas()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.bg.atlas.md),
[`collayer.bg.meancurv()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.bg.meancurv.md),
[`collayer.bg.sulc()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.bg.sulc.md),
[`collayer.from.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.from.annot.md),
[`collayer.from.annotdata()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.from.annotdata.md),
[`collayer.from.mask.data()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.from.mask.data.md),
[`collayer.from.morphlike.data()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.from.morphlike.data.md),
[`collayers.merge()`](https://dfsp-spirit.github.io/fsbrain/reference/collayers.merge.md)
