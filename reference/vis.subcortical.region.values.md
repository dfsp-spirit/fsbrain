# Visualize one value per region of the subcortical atlas of a subject.

Render the subcortical structures of a subject and assign one color per
structure, based on one value per atlas region. The subcortical atlas is
not defined on the cortical surface: it comes with its own surface mesh
that contains the 8 subcortical structures per hemisphere (accumbens
area, amygdala, caudate, hippocampus, pallidum, putamen, thalamus and
lateral ventricle). The mesh and the annotation file are expected in the
subject directory ('surf/lh.subcortical', 'surf/rh.subcortical',
'label/lh.subcortical.annot', 'label/rh.subcortical.annot').

The atlas files for the fsaverage template subject are not part of
FreeSurfer and are not required for the package to work. They can be
downloaded with
[`download_optional_data`](https://dfsp-spirit.github.io/fsbrain/reference/download_optional_data.md)
or
[`download_fsaverage_atlases`](https://dfsp-spirit.github.io/fsbrain/reference/download_fsaverage_atlases.md)
into the package cache, which is searched for a subject named
'fsaverage' by default.

Optionally, the structures can be rendered inside a semi-transparent
context mesh, typically the cortex of the same subject (see parameter
'cortex'). Note that the context mesh and the atlas mesh must be defined
in the same coordinate space, which is why both are taken from the same
subject by default.

## Usage

``` r
vis.subcortical.region.values(
  subjects_dir = NULL,
  subject_id = "fsaverage",
  lh_region_value_list,
  rh_region_value_list,
  atlas = "subcortical",
  surface = "subcortical",
  cortex = NULL,
  views = c("sd_lateral_lh", "sd_medial_lh", "sd_lateral_rh", "sd_medial_rh"),
  rgloptions = rglo(),
  rglactions = list(),
  value_for_unlisted_regions = NA,
  draw_colorbar = FALSE,
  makecmap_options = mkco.seq(),
  style = "default",
  silent = FALSE
)
```

## Arguments

- subjects_dir:

  string or `NULL`. The FreeSurfer SUBJECTS_DIR, i.e., a directory
  containing the data for all your subjects, each in a subdir named
  after the subject identifier. If `NULL`, the locations searched by
  [`find.subjectsdir.of`](https://dfsp-spirit.github.io/fsbrain/reference/find.subjectsdir.of.md)
  (package cache and FreeSurfer/SUBJECTS_DIR configuration) are checked
  for one that contains the atlas files, and the first such location is
  used.

- subject_id:

  string. The subject identifier. Defaults to 'fsaverage', the template
  subject for which the subcortical atlas is available for download.

- lh_region_value_list:

  named list. A list for the left hemisphere in which the names are
  atlas regions, and the values are the value to write to all vertices
  of that region, see
  [`vis.region.values.on.subject`](https://dfsp-spirit.github.io/fsbrain/reference/vis.region.values.on.subject.md).
  Use `NaN` as the value of a region to hide it completely: the vertices
  of that region are removed from the mesh and it is not rendered at
  all, see the 'details' section.

- rh_region_value_list:

  named list, the same for the right hemisphere.

- atlas:

  string. The name of the atlas to use. Defaults to 'subcortical', the
  subcortical atlas that ships with the package. Used to construct the
  annotation file name.

- surface:

  string. The name of the surface mesh that belongs to the atlas.
  Defaults to 'subcortical'. Used to construct the surface file name, in
  contrast to the other vis functions this is not a cortical surface.

- cortex:

  `NULL` or the definition of a context mesh to render the structures
  in, typically a semi-transparent cortex of the same subject. Supported
  values are a character string (the surface name, e.g., 'white' or
  'pial'), a named list of options for the context mesh (entries
  'surface', 'color', 'alpha', 'style', 'subjects_dir' and
  'subject_id'), an
  [`coloredmesh.from.color`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.color.md)
  instance, or a hemilist of such instances. Use `NULL` to render the
  structures without any context.

- views:

  list of strings. The views to render. Defaults to lateral and medial
  views of both hemispheres. See
  [`get.view.angle.names`](https://dfsp-spirit.github.io/fsbrain/reference/get.view.angle.names.md)
  for valid entries.

- rgloptions:

  option list passed to
  [`par3d`](https://dmurdoch.github.io/rgl/dev/reference/par3d.html).
  Example: `rgloptions = list("windowRect"=c(50,50,1000,1000))`.

- rglactions:

  named list. A list in which the names are from a set of pre-defined
  actions, see
  [`rglactions`](https://dfsp-spirit.github.io/fsbrain/reference/rglactions.md).
  Note that the action 'shift_hemis_apart' is not supported here: the
  structures of a mesh atlas are rendered in their anatomical position.

- value_for_unlisted_regions:

  numerical scalar or `NA`, the value to assign to regions which do not
  occur in the region value lists, see
  [`vis.region.values.on.subject`](https://dfsp-spirit.github.io/fsbrain/reference/vis.region.values.on.subject.md).
  Set this to `NaN` to hide all regions that are not listed explicitly.

- draw_colorbar:

  logical. Whether to draw a colorbar. Defaults to FALSE, see
  [`coloredmesh.plot.colorbar.separate`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.plot.colorbar.separate.md)
  for a better looking alternative.

- makecmap_options:

  named list of parameters to pass to
  [`makecmap`](https://rdrr.io/pkg/squash/man/makecmap.html). Must not
  include the unnamed first parameter, which is derived from the data.

- style:

  a rendering style for the data meshes, see
  [`get.rglstyle`](https://dfsp-spirit.github.io/fsbrain/reference/get.rglstyle.md).
  Defaults to 'default'. The context mesh defined via parameter 'cortex'
  has its own style, see there.

- silent:

  logical, whether to suppress messages.

## Value

list of coloredmeshes. The coloredmeshes used for the visualization,
invisibly. The list contains the data meshes (and the context meshes, if
any) as a flat list, so it can be passed to
[`export`](https://dfsp-spirit.github.io/fsbrain/reference/export.md) to
save the rendered views into an image file.

## Note

The subcortical atlas is defined in MNI305 space (fsaverage surface
RAS), so it can be combined with the cortical surfaces of the fsaverage
template subject. The 8 structures per hemisphere are colored with the
standard FreeSurfer 'aseg' colors when you visualize the atlas itself,
this function assigns data-driven colors instead. Region names are the
FreeSurfer 'aseg' structure names, e.g., 'Left-Hippocampus' or
'Right-Thalamus-Proper'.

This function is not limited to the subcortical atlas: any atlas that
comes with its own mesh and annotation files works, pass the respective
names via parameters 'atlas' and 'surface'.

## Hiding regions

Assigning the value `NaN` to a region hides it: the vertices of the
region are removed from the mesh (together with the faces that use
them), so the structure is not rendered at all, in contrast to drawing
it in the color that represents missing data. This is a per-vertex
operation on the shared mesh of all regions of a hemisphere, so hiding
is not limited to entire meshes and can be combined with any rendering
style.

The values of hidden regions are excluded from the colorbar range, just
like `NA` values. Setting `value_for_unlisted_regions = NaN` hides all
regions that are not listed in the region value lists, which is a
convenient way of visualizing only a few structures of an atlas.

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
[`vis.subject.pre()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.pre.md),
[`vis.symmetric.data.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.symmetric.data.on.subject.md),
[`vis.volume.clusters()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.volume.clusters.md),
[`vis.volume.on.surface()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.volume.on.surface.md),
[`vislayout.from.coloredmeshes()`](https://dfsp-spirit.github.io/fsbrain/reference/vislayout.from.coloredmeshes.md)

Other region-based visualization functions:
[`vis.region.values.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.region.values.on.subject.md),
[`vis.subject.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.annot.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_optional_data();   # includes the subcortical atlas for fsaverage
   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");

   # One value per region, for all 8 subcortical structures of the left and right hemisphere.
   lh_region_values = list("Left-Accumbens-area"=0.1, "Left-Amygdala"=0.2,
    "Left-Caudate"=0.3, "Left-Hippocampus"=0.4, "Left-Pallidum"=0.5,
    "Left-Putamen"=0.6, "Left-Thalamus-Proper"=0.7, "Left-Lateral-Ventricle"=0.8);
   rh_region_values = list("Right-Accumbens-area"=0.1, "Right-Amygdala"=0.2,
    "Right-Caudate"=0.3, "Right-Hippocampus"=0.4, "Right-Pallidum"=0.5,
    "Right-Putamen"=0.6, "Right-Thalamus-Proper"=0.7, "Right-Lateral-Ventricle"=0.8);

   # Render the structures on their own, and save the result to a file:
   cm = vis.subcortical.region.values(subjects_dir, "fsaverage", lh_region_values,
    rh_region_values, rglactions = list("no_vis" = TRUE));
   export(cm, colorbar_legend = "my values", output_img = "subcortical.png");

   # Render the structures inside a semi-transparent cortex:
   cm_ctx = vis.subcortical.region.values(subjects_dir, "fsaverage", lh_region_values,
    rh_region_values, cortex = "white", rglactions = list("no_vis" = TRUE));
   export(cm_ctx, colorbar_legend = "my values", output_img = "subcortical_in_cortex.png");

   # Visualize only the two hippocampi, by hiding all other regions (they are set to NaN):
   cm_hippo = vis.subcortical.region.values(subjects_dir, "fsaverage",
    list("Left-Hippocampus" = 0.2), list("Right-Hippocampus" = 0.8),
    value_for_unlisted_regions = NaN, rglactions = list("no_vis" = TRUE));
   export(cm_hippo, colorbar_legend = "my values", output_img = "subcortical_hippocampi.png");
} # }
```
