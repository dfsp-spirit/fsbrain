# Visualize native space morphometry data for a subject.

Creates a surface mesh, applies a colormap transform the morphometry
data values into colors, and renders the resulting colored mesh in an
interactive window. If hemi is 'both', the data is rendered for the
whole brain.

## Usage

``` r
vis.subject.morph.native(
  subjects_dir,
  subject_id,
  measure,
  hemi = "both",
  surface = "white",
  views = c("t4"),
  rgloptions = rglo(),
  rglactions = list(),
  draw_colorbar = FALSE,
  cortex_only = FALSE,
  style = "default",
  makecmap_options = mkco.seq(),
  bg = NULL
)
```

## Arguments

- subjects_dir:

  string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the
  data for all your subjects, each in a subdir named after the subject
  identifier.

- subject_id:

  string. The subject identifier.

- measure:

  string. The morphometry data to use. E.g., 'area' or 'thickness'. Pass
  NULL to render just the surface in white, without any data.

- hemi:

  string, one of 'lh', 'rh', or 'both'. The hemisphere name. Used to
  construct the names of the label data files to be loaded.

- surface:

  string. The display surface. E.g., "white", "pial", or "inflated".
  Defaults to "white".

- views:

  list of strings. Valid entries include: 'si': single interactive view.
  't4': tiled view showing the brain from 4 angles. 't9': tiled view
  showing the brain from 9 angles.

- rgloptions:

  option list passed to
  [`par3d`](https://dmurdoch.github.io/rgl/dev/reference/par3d.html).
  Example: `rgloptions = list("windowRect"=c(50,50,1000,1000))`.

- rglactions:

  named list. A list in which the names are from a set of pre-defined
  actions. The values can be used to specify parameters for the action.
  The following example clips outliers in the data before plotting and
  writes a screenshot in PNG format:
  `rglactions = list("snapshot_png"="~/fsbrain.png", "clip_data"=c(0.05, 0.95))`.
  See
  [`rglactions`](https://dfsp-spirit.github.io/fsbrain/reference/rglactions.md).

- draw_colorbar:

  logical or one of the character strings 'vertical' or 'horizontal',
  whether to draw a colorbar. Notice: the colorbar is drawn to a
  separate subplot, and this only works if there is enough space for it,
  i.e., the plot resolution must be high enough. You may have to
  increase the plot size for the colorbar to show up, see the vignette
  for instructions. Defaults to `FALSE`. See
  [`coloredmesh.plot.colorbar.separate`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.plot.colorbar.separate.md)
  for an alternative.

- cortex_only:

  logical, whether to mask the medial wall, i.e., whether the
  morphometry data for all vertices which are *not* part of the cortex
  (as defined by the label file `label/?h.cortex.label`) should be
  replaced with NA values. In other words, setting this to TRUE will
  ignore the values of the medial wall between the two hemispheres. If
  set to true, the mentioned label file needs to exist for the subject.
  Defaults to FALSE.

- style:

  character string, a rendering style, e.g., 'default', 'shiny' or
  'semitransparent'.

- makecmap_options:

  named list of parameters to pass to
  [`makecmap`](https://rdrr.io/pkg/squash/man/makecmap.html). Must not
  include the unnamed first parameter, which is derived from 'measure'.
  Should include at least a colormap function as name 'colFn'.

- bg:

  a background definition. Can be a surface color layer or a character
  string like 'curv_light' to select a pre-defined layer, see
  [`collayer.bg`](https://dfsp-spirit.github.io/fsbrain/reference/collayer.bg.md)
  for valid strings.

## Value

list of coloredmeshes. The coloredmeshes used for the visualization.

## See also

Other visualization functions:
[`highlight.vertices.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/highlight.vertices.on.subject.md),
[`highlight.vertices.on.subject.spheres()`](https://dfsp-spirit.github.io/fsbrain/reference/highlight.vertices.on.subject.spheres.md),
[`vis.color.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.color.on.subject.md),
[`vis.data.on.fsaverage()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.data.on.fsaverage.md),
[`vis.data.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.data.on.subject.md),
[`vis.labeldata.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.labeldata.on.subject.md),
[`vis.mask.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.mask.on.subject.md),
[`vis.region.values.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.region.values.on.subject.md),
[`vis.rglwidget()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.rglwidget.md),
[`vis.subject.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.annot.md),
[`vis.subject.label()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.label.md),
[`vis.subject.morph.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.morph.standard.md),
[`vis.subject.pre()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.pre.md),
[`vis.symmetric.data.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.symmetric.data.on.subject.md),
[`vis.volume.on.surface()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.volume.on.surface.md),
[`vislayout.from.coloredmeshes()`](https://dfsp-spirit.github.io/fsbrain/reference/vislayout.from.coloredmeshes.md)

Other morphometry visualization functions:
[`vis.data.on.fsaverage()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.data.on.fsaverage.md),
[`vis.data.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.data.on.subject.md),
[`vis.subject.morph.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.morph.standard.md),
[`vis.symmetric.data.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.symmetric.data.on.subject.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_optional_data();
   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
   vis.subject.morph.native(subjects_dir, 'subject1', 'thickness', 'lh', views=c("t9"));
} # }
```
