# Visualize a vertex mask on the surface of a subject.

A mask is a logical vector that contains one value per vertex. You can
create it manually, or use functions like
[`mask.from.labeldata.for.hemi`](https://dfsp-spirit.github.io/fsbrain/reference/mask.from.labeldata.for.hemi.md)
to create and modify it. Check the example for this function.

## Usage

``` r
vis.mask.on.subject(
  subjects_dir,
  vis_subject_id,
  mask_lh,
  mask_rh,
  surface = "white",
  views = c("t4"),
  rgloptions = rglo(),
  rglactions = list(),
  draw_colorbar = FALSE,
  makecmap_options = list(colFn = label.colFn.inv),
  style = "default"
)
```

## Arguments

- subjects_dir:

  string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the
  data for all your subjects, each in a subdir named after the subject
  identifier.

- vis_subject_id:

  string. The subject identifier from which to obtain the surface for
  data visualization. Example: 'fsaverage'.

- mask_lh:

  logical vector or NULL, the mask to visualize on the left hemisphere
  surface. Must have the same length as the lh surface of the
  vis_subject_id has vertices. If NULL, this surface will not be
  rendered. Only one of mask_lh or mask_rh is allowed to be NULL.

- mask_rh:

  logical vector or NULL, the mask to visualize on the right hemisphere
  surface. Must have the same length as the rh surface of the
  vis_subject_id has vertices. If NULL, this surface will not be
  rendered. Only one of mask_lh or mask_rh is allowed to be NULL.

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

- makecmap_options:

  named list of parameters to pass to
  [`makecmap`](https://rdrr.io/pkg/squash/man/makecmap.html). Must not
  include the unnamed first parameter, which is derived from 'measure'.
  Should include at least a colormap function as name 'colFn'.

- style:

  character string, a rendering style, e.g., 'default', 'shiny' or
  'semitransparent'.

## Value

list of coloredmeshes. The coloredmeshes used for the visualization.

## Note

Drawing a colorbar for label data makes limited sense, use a legend
instead. The colorbar can give a rough overview of the relative number
of label and non-label vertices though, so it is possible to request
one.

## See also

Other mask functions:
[`coloredmesh.from.mask()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.mask.md),
[`mask.from.labeldata.for.hemi()`](https://dfsp-spirit.github.io/fsbrain/reference/mask.from.labeldata.for.hemi.md)

Other visualization functions:
[`highlight.vertices.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/highlight.vertices.on.subject.md),
[`highlight.vertices.on.subject.spheres()`](https://dfsp-spirit.github.io/fsbrain/reference/highlight.vertices.on.subject.spheres.md),
[`vis.color.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.color.on.subject.md),
[`vis.data.on.fsaverage()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.data.on.fsaverage.md),
[`vis.data.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.data.on.subject.md),
[`vis.labeldata.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.labeldata.on.subject.md),
[`vis.region.values.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.region.values.on.subject.md),
[`vis.rglwidget()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.rglwidget.md),
[`vis.subject.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.annot.md),
[`vis.subject.label()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.label.md),
[`vis.subject.morph.native()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.morph.native.md),
[`vis.subject.morph.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.morph.standard.md),
[`vis.subject.pre()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.pre.md),
[`vis.symmetric.data.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.symmetric.data.on.subject.md),
[`vis.volume.on.surface()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.volume.on.surface.md),
[`vislayout.from.coloredmeshes()`](https://dfsp-spirit.github.io/fsbrain/reference/vislayout.from.coloredmeshes.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_optional_data();

  # Define the data to use:
  subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
  subject_id = 'subject1';
  surface = 'white';
  hemi = 'both';
  atlas = 'aparc';
  region = 'bankssts';

  # Create a mask from a region of an annotation:
  lh_annot = subject.annot(subjects_dir, subject_id, 'lh', atlas);
  rh_annot = subject.annot(subjects_dir, subject_id, 'rh', atlas);
  lh_label = label.from.annotdata(lh_annot, region);
  rh_label = label.from.annotdata(rh_annot, region);
  lh_mask = mask.from.labeldata.for.hemi(lh_label, length(lh_annot$vertices));
  rh_mask = mask.from.labeldata.for.hemi(rh_label, length(rh_annot$vertices));

  # Edit the mask: add the vertices from another region to it:
  region2 = 'medialorbitofrontal';
  lh_label2 = label.from.annotdata(lh_annot, region2);
  rh_label2 = label.from.annotdata(rh_annot, region2);
  lh_mask2 = mask.from.labeldata.for.hemi(lh_label2, length(lh_annot$vertices),
   existing_mask = lh_mask);
  rh_mask2 = mask.from.labeldata.for.hemi(rh_label2, length(rh_annot$vertices),
   existing_mask = rh_mask);
  # Visualize the mask:
  vis.mask.on.subject(subjects_dir, subject_id, lh_mask2, rh_mask2);
} # }
```
