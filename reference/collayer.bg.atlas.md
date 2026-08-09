# Compute atlas or annotation surface color layer.

Compute atlas or annotation surface color layer.

## Usage

``` r
collayer.bg.atlas(
  subjects_dir,
  subject_id,
  hemi = "both",
  atlas = "aparc",
  grayscale = FALSE,
  outline = FALSE,
  outline_surface = "white"
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

- atlas:

  character string, the atlas name. E.g., "aparc", "aparc.2009s", or
  "aparc.DKTatlas". Used to construct the name of the annotation file to
  be loaded.

- grayscale:

  logical, whether to convert the atlas colors to grayscale

- outline:

  logical, whether to draw an outline only instead of filling the
  regions. Defaults to `FALSE`. Instead of passing `TRUE`, one can also
  pass a list of extra parameters to pass to
  [`annot.outline`](https://dfsp-spirit.github.io/fsbrain/reference/annot.outline.md),
  e.g., `outline=list('outline_color'='#000000')`.

- outline_surface:

  character string, the surface to load. Only relevant when 'outline' is
  used. (In that case the surface mesh is needed to compute the vertices
  forming the region borders.)

## Value

a color layer, i.e., vector of color strings in a hemilist

## Note

Using 'outline' mode is quite slow, and increasing the border thickness
makes it even slower.

## See also

You can plot the return value using
[`vis.color.on.subject`](https://dfsp-spirit.github.io/fsbrain/reference/vis.color.on.subject.md).

Other surface color layer:
[`collayer.bg()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.bg.md),
[`collayer.bg.meancurv()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.bg.meancurv.md),
[`collayer.bg.sulc()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.bg.sulc.md),
[`collayer.from.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.from.annot.md),
[`collayer.from.annotdata()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.from.annotdata.md),
[`collayer.from.mask.data()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.from.mask.data.md),
[`collayer.from.morphlike.data()`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.from.morphlike.data.md),
[`collayers.merge()`](https://dfsp-spirit.github.io/fsbrain/reference/collayers.merge.md)
