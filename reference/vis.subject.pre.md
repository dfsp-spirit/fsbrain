# Visualize pre-loaded data.

Visualize pre-loaded data.

## Usage

``` r
vis.subject.pre(
  surfaces,
  pervertex_data,
  hemi = "both",
  views = c("t4"),
  rgloptions = rglo(),
  rglactions = list(),
  draw_colorbar = FALSE,
  style = "default",
  makecmap_options = mkco.seq()
)
```

## Arguments

- surfaces:

  a
  [`hemilist`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.md)
  of surfaces loaded with a function like
  [`freesurferformats::read.fs.surface`](https://rdrr.io/pkg/freesurferformats/man/read.fs.surface.html).

- pervertex_data:

  a
  [`hemilist`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.md)
  of per-vertex data for the surfaces, i.e., a list of numeric vectors.
  E.g., loaded from a moorphometry data file with a function like
  [`freesurferformats::read.fs.morph`](https://rdrr.io/pkg/freesurferformats/man/read.fs.morph.html).
  ´

- hemi:

  string, one of 'lh', 'rh', or 'both'. The hemisphere name. Used to
  construct the names of the label data files to be loaded.

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

- style:

  character string, a rendering style, e.g., 'default', 'shiny' or
  'semitransparent'.

- makecmap_options:

  named list of parameters to pass to
  [`makecmap`](https://rdrr.io/pkg/squash/man/makecmap.html). Must not
  include the unnamed first parameter, which is derived from 'measure'.
  Should include at least a colormap function as name 'colFn'.

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
[`vis.subject.morph.native()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.morph.native.md),
[`vis.subject.morph.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.morph.standard.md),
[`vis.symmetric.data.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.symmetric.data.on.subject.md),
[`vis.volume.on.surface()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.volume.on.surface.md),
[`vislayout.from.coloredmeshes()`](https://dfsp-spirit.github.io/fsbrain/reference/vislayout.from.coloredmeshes.md)
