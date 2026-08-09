# Visualize a label on the surface of a subject.

Visualizes a label. Note that a label is just a set of vertices, and
that you can use this function to visualize sets of vertices, e.g., to
see where on the mesh a certain vertex lies. It may be helpful the
visualize the vertex with its neighbors, because otherwise it may be too
small to spot. Use the function
[mesh.vertex.neighbors](https://dfsp-spirit.github.io/fsbrain/reference/mesh.vertex.neighbors.md)
to get them. It is advisable to set the view to the interactive 'si'
mode and use the 'inflated' surface to identify single vertices.

## Usage

``` r
vis.labeldata.on.subject(
  subjects_dir,
  vis_subject_id,
  lh_labeldata,
  rh_labeldata,
  surface = "white",
  views = c("t4"),
  rgloptions = rglo(),
  rglactions = list(),
  draw_colorbar = FALSE,
  makecmap_options = list(colFn = label.colFn.inv),
  style = "default",
  ...
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

- lh_labeldata:

  integer vector of vertex indices for the left hemisphere

- rh_labeldata:

  integer vector of vertex indices for the right hemisphere

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

- ...:

  extra arguments to pass to
  [`coloredmesh.from.label`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.label.md).

## Value

list of coloredmeshes. The coloredmeshes used for the visualization.

## Note

Drawing a colorbar for label data makes limited sense, use a legend
instead. The colorbar can give a rough overview of the relative number
of label and non-label vertices though, so it is possible to request
one.

## See also

Other label functions:
[`apply.label.to.morphdata()`](https://dfsp-spirit.github.io/fsbrain/reference/apply.label.to.morphdata.md),
[`apply.labeldata.to.morphdata()`](https://dfsp-spirit.github.io/fsbrain/reference/apply.labeldata.to.morphdata.md),
[`subject.lobes()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.lobes.md),
[`subject.mask()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.mask.md),
[`vis.subject.label()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.label.md)

Other visualization functions:
[`highlight.vertices.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/highlight.vertices.on.subject.md),
[`highlight.vertices.on.subject.spheres()`](https://dfsp-spirit.github.io/fsbrain/reference/highlight.vertices.on.subject.spheres.md),
[`vis.color.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.color.on.subject.md),
[`vis.data.on.fsaverage()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.data.on.fsaverage.md),
[`vis.data.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.data.on.subject.md),
[`vis.mask.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.mask.on.subject.md),
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
  lh_labeldata = c(1000, 1001, 1002);   # only the vertices, will be tiny.
  subject_id = 'subject1';
  surface = 'white'; # Should use 'inflated', but we do not currently
                     # ship it for the example subject to reduce download size.

  # For the right hemi, extend them to neighborhood for better visibility:
  rh_labeldata = c(500, 5000);
  rh_surface = subject.surface(subjects_dir, subject_id, surface, 'rh');
  rh_labeldata_neighborhood = mesh.vertex.neighbors(rh_surface, rh_labeldata);
  vis.labeldata.on.subject(subjects_dir, subject_id, lh_labeldata,
   rh_labeldata_neighborhood$vertices, surface=surface, views=c('si'));
} # }
```
