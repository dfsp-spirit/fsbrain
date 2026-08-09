# Visualize arbitrary data, one value per atlas region, on the surface of any subject (including template subjects).

This function can be used for rendering a single value (color) for all
vertices of an atlas region. The typical usecase is the visualization of
results of atlas-based analyses, e.g., p-value, means or other
aggregated values over all vertices of a region.

## Usage

``` r
vis.region.values.on.subject(
  subjects_dir,
  subject_id,
  atlas,
  lh_region_value_list,
  rh_region_value_list,
  surface = "white",
  views = c("t4"),
  rgloptions = rglo(),
  rglactions = list(),
  value_for_unlisted_regions = NA,
  draw_colorbar = FALSE,
  makecmap_options = mkco.heat(),
  bg = NULL,
  silent = FALSE,
  style = "default",
  border = NULL
)
```

## Arguments

- subjects_dir:

  string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the
  data for all your subjects, each in a subdir named after the subject
  identifier.

- subject_id:

  string. The subject identifier.

- atlas:

  string. The brain atlas to use. E.g., 'aparc' or 'aparc.a2009s'.

- lh_region_value_list:

  named list. A list for the left hemisphere in which the names are
  atlas regions, and the values are the value to write to all vertices
  of that region. You can pass an unnamed list, but then the its length
  must exactly match the number of atlas regions. The order of values
  must also match the order of regions in the annotation, of course. The
  resulting mapping will be printed so you can check it (unless 'silent'
  is set).

- rh_region_value_list:

  named list. A list for the right hemisphere in which the names are
  atlas regions, and the values are the value to write to all vertices
  of that region.

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

- value_for_unlisted_regions:

  numerical scalar or `NA`, the value to assign to regions which do not
  occur in the region_value_lists. Defaults to `NA`.

- draw_colorbar:

  logical or one of the character strings 'vertical' or 'horizontal',
  whether to draw a colorbar. Notice: the colorbar is drawn to a
  separate subplot, and this only works if there is enough space for it,
  i.e., the plot resolution must be high enough. You may have to
  increase the plot size for the colorbar to show up, see the vignette
  for instructions. Defaults to `FALSE`. See
  [`coloredmesh.plot.colorbar.separate`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.plot.colorbar.separate.md)
  for an alternative.

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

- silent:

  logical, whether to suppress mapping info in case of unnamed region
  value lists (see 'lh_region_value_list' description).

- style:

  character string, a rendering style, e.g., 'default', 'shiny' or
  'semitransparent'.

- border:

  logical, whether to add a black border around the regions.
  Alternatively, the parameter can be given as a named list with entries
  'color' and 'expand_inwards', where the latter defines the borders
  thickness. E.g.,
  `border = list('color'='#FF0000', 'expand_inwards'=2L)`. Border
  computation is slow, sorry.

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
[`vis.rglwidget()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.rglwidget.md),
[`vis.subject.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.annot.md),
[`vis.subject.label()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.label.md),
[`vis.subject.morph.native()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.morph.native.md),
[`vis.subject.morph.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.morph.standard.md),
[`vis.subject.pre()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.pre.md),
[`vis.symmetric.data.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.symmetric.data.on.subject.md),
[`vis.volume.on.surface()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.volume.on.surface.md),
[`vislayout.from.coloredmeshes()`](https://dfsp-spirit.github.io/fsbrain/reference/vislayout.from.coloredmeshes.md)

Other region-based visualization functions:
[`vis.subject.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.annot.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_optional_data();
   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
   atlas = 'aparc';   # Desikan atlas
   # For the left hemisphere, we just assign a subset of the
   # atlas regions. The others will get the default value.
   lh_region_value_list = list("bankssts"=0.9, "precuneus"=0.7, "postcentral"=0.8, "lingual"=0.6);
   # For the right hemisphere, we retrieve the full list of regions for
   # the atlas, and assign random values to all of them.
   atlas_region_names = get.atlas.region.names(atlas, template_subjects_dir = subjects_dir,
    template_subject='subject1');
   rh_region_value_list = rnorm(length(atlas_region_names), 3.0, 1.0);
   names(rh_region_value_list) = atlas_region_names;
   vis.region.values.on.subject(subjects_dir, 'subject1', atlas,
    lh_region_value_list, rh_region_value_list);
} # }
```
