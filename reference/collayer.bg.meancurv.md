# Compute binarized mean curvature surface color layer.

Compute a binarized mean curvature surface color layer, this is intended
as a background color layer. You can merge it with your data layer using
[`collayers.merge`](https://dfsp-spirit.github.io/fsbrain/reference/collayers.merge.md).

## Usage

``` r
collayer.bg.meancurv(
  subjects_dir,
  subject_id,
  hemi = "both",
  cortex_only = FALSE,
  bin_colors = c("#898989", "#5e5e5e"),
  bin_thresholds = c(0)
)
```

## Arguments

- subjects_dir:

  character string, the FreeSurfer SUBJECTS_DIR.

- subject_id:

  character string, the subject identifier.

- hemi:

  character string, one of 'lh', 'rh', or 'both'. The latter will merge
  the data for both hemis into a single vector.

- cortex_only:

  logical, whether to restrict pattern computation to the cortex.

- bin_colors:

  vector of two character strings, the two colors to use.

- bin_thresholds:

  vector of 1 or 2 double values, the curvature threshold values used to
  separate gyri from sulci.

## Value

a color layer, i.e., vector of color strings in a hemilist

## See also

You can plot the return value using
[`vis.color.on.subject`](https://dfsp-spirit.github.io/fsbrain/reference/vis.color.on.subject.md).

Other surface color layer:
[`collayer.bg()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.bg.md),
[`collayer.bg.atlas()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.bg.atlas.md),
[`collayer.bg.sulc()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.bg.sulc.md),
[`collayer.from.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.from.annot.md),
[`collayer.from.annotdata()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.from.annotdata.md),
[`collayer.from.mask.data()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.from.mask.data.md),
[`collayer.from.morphlike.data()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.from.morphlike.data.md),
[`collayers.merge()`](https://dfsp-spirit.github.io/fsbrain/reference/collayers.merge.md)
