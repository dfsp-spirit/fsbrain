# Visualize pre-defined vertex colors on a subject.

Visualize pre-defined vertex colors on a subject.

## Usage

``` r
vis.color.on.subject(
  subjects_dir,
  vis_subject_id,
  color_lh = NULL,
  color_rh = NULL,
  surface = "white",
  views = c("t4"),
  rgloptions = rglo(),
  rglactions = list(),
  color_both = NULL,
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

- color_lh:

  vector of colors to visualize on the left hemisphere surface. Length
  must match number of vertices in hemi surface, or be a single color.

- color_rh:

  vector of colors to visualize on the right hemisphere surface. Length
  must match number of vertices in hemi surface, or be a single color.

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

- color_both:

  vector of colors to visualize on the left and right hemispheres.
  Alternative to 'color_lh' and 'color_rh'. Length must match sum of
  vertices in both hemis. Can also be a hemilist.

- style:

  character string or rgl rendering style, see
  [`get.rglstyle`](https://dfsp-spirit.github.io/fsbrain/reference/get.rglstyle.md).

## Value

list of coloredmeshes. The coloredmeshes used for the visualization.

## See also

Other visualization functions:
[`highlight.vertices.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/highlight.vertices.on.subject.md),
[`highlight.vertices.on.subject.spheres()`](https://dfsp-spirit.github.io/fsbrain/reference/highlight.vertices.on.subject.spheres.md),
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
[`vis.volume.on.surface()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.volume.on.surface.md),
[`vislayout.from.coloredmeshes()`](https://dfsp-spirit.github.io/fsbrain/reference/vislayout.from.coloredmeshes.md)

Other surface visualization functions:
[`highlight.vertices.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/highlight.vertices.on.subject.md),
[`highlight.vertices.on.subject.spheres()`](https://dfsp-spirit.github.io/fsbrain/reference/highlight.vertices.on.subject.spheres.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_optional_data();
   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
   color_lh = '#ff0000';
   num_verts_subject1_rh = 153333;
   color_rh = rep('#333333', num_verts_subject1_rh);
   color_rh[1:30000] = '#00ff00';
   color_rh[30001:60000] = '#ff0000';
   color_rh[60001:90000] = '#0000ff';
   color_rh[90001:120000] = '#ffff00';
   color_rh[120001:150000] = '#00ffff';
   vis.color.on.subject(subjects_dir, 'subject1', color_lh, color_rh);
} # }
```
