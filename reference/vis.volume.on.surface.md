# Visualize a brain volume overlaid on a cortical surface in 3D.

Render a brain volume (as an isosurface or as voxels) together with a
cortical surface mesh colored by morphometry data in the same
interactive 3D scene. The volume coordinates are transformed to surface
RAS space using the FreeSurfer `vox2ras_tkr` matrix, ensuring proper
spatial alignment of volume and surface.

## Usage

``` r
vis.volume.on.surface(
  subjects_dir,
  subject_id,
  volume = "brain",
  volume_mode = "contour",
  volume_level = 80,
  volume_color = "#666666",
  volume_alpha = 0.3,
  measure = "thickness",
  surface = "white",
  hemi = "both",
  views = c("t4"),
  surface_style = "semitransparent",
  rgloptions = rglo(),
  makecmap_options = mkco.seq(),
  cortex_only = FALSE,
  render_every = 20L,
  ...
)
```

## Arguments

- subjects_dir:

  character string, the FreeSurfer SUBJECTS_DIR, i.e., a directory
  containing the data for all your subjects, each in a subdir named
  after the subject identifier.

- subject_id:

  character string, the subject identifier.

- volume:

  numeric 3D array or character string. Either a 3D volume array with
  voxel intensities, or the name of a volume file to load from the
  subject's `mri/` directory (e.g., `"brain"`, `"aseg"`,
  `"aparc+aseg"`). Background voxels should have value `NA` or 0.

- volume_mode:

  character string, one of `"contour"` or `"voxels"`. The rendering mode
  for the volume: `"contour"` creates a smooth isosurface using the
  `misc3d` package (requires the optional dependency `misc3d`),
  `"voxels"` renders individual foreground voxels as small cubes.
  Defaults to `"contour"`.

- volume_level:

  numeric scalar, the intensity threshold for contour mode. Only voxels
  with intensity values \>= this threshold contribute to the isosurface.
  Ignored in voxel mode. Defaults to 80 (suitable for a raw brain.mgz
  with values in range 0-255).

- volume_color:

  character string, the color for the volume rendering. Defaults to
  `"#666666"` (medium gray).

- volume_alpha:

  numeric in range 0..1, the transparency (alpha) of the volume overlay.
  Lower values make the volume more transparent, revealing the surface
  beneath. Defaults to 0.3.

- measure:

  character string or NULL, the morphometry data to use for coloring the
  surface. E.g., `"thickness"`, `"sulc"`, `"area"`, or `"curv"`. Pass
  `NULL` to render the surface in a single plain color without
  morphometry overlay. Defaults to `"thickness"`.

- surface:

  character string, the display surface. E.g., `"white"`, `"pial"`, or
  `"inflated"`. Defaults to `"white"`.

- hemi:

  character string, one of `'lh'`, `'rh'`, or `'both'`. The hemisphere
  to display. Defaults to `"both"`.

- views:

  list of strings, the view configuration. For single interactive view
  use `c("si")`. For 4-angle tiled view use `c("t4")`. For 9-angle tiled
  view use `c("t9")`. Defaults to `c("t4")`.

- surface_style:

  character string, a rendering style for the cortical surface, e.g.,
  `'default'`, `'shiny'` or `'semitransparent'`. Use `'semitransparent'`
  to make the surface partially see-through, which works well for volume
  overlays. Defaults to `"semitransparent"`.

- rgloptions:

  named list, parameters passed to
  [`par3d`](https://dmurdoch.github.io/rgl/dev/reference/par3d.html).
  Example: `rgloptions = list("windowRect"=c(50,50,800,800))`. Defaults
  to the result of
  [`rglo`](https://dfsp-spirit.github.io/fsbrain/reference/rglo.md).

- makecmap_options:

  named list of parameters to pass to
  [`makecmap`](https://rdrr.io/pkg/squash/man/makecmap.html). Should
  include at least a colormap function as name `'colFn'`. Defaults to
  the result of
  [`mkco.seq`](https://dfsp-spirit.github.io/fsbrain/reference/mkco.seq.md).

- cortex_only:

  logical, whether to mask the medial wall (i.e., only render cortical
  vertices). Defaults to `FALSE`.

- render_every:

  integer, for voxel mode only: render every Nth foreground voxel.
  Higher values improve performance but reduce density. Set to 1 to
  render all voxels (may be slow). Defaults to 20.

- ...:

  extra parameters passed to the volume rendering backend (e.g.,
  `lit=FALSE` for unlit voxels).

## Value

invisible named list with entries: `"surface"` (the coloredmeshes from
the surface rendering, a hemilist of `coloredmesh` instances) and
`"volume"` (the volume rendering result: a `Triangles3D` object for
contour mode, or a list of `coloredvoxels` for voxel mode).

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
[`vislayout.from.coloredmeshes()`](https://dfsp-spirit.github.io/fsbrain/reference/vislayout.from.coloredmeshes.md)

Other volume visualization:
[`volvis.lb()`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.lb.md),
[`volvis.lb.with.surface()`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.lb.with.surface.md),
[`volvis.lightbox()`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.lightbox.md),
[`volvis.slices.with.surface()`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.slices.with.surface.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_optional_data();
   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
   # Contour overlay of brain volume on thickness-colored surface:
   vis.volume.on.surface(subjects_dir, 'subject1', 'brain',
      volume_mode='contour', volume_level=60, measure='thickness',
      views=c('si'), surface_style='semitransparent');
   # Voxel overlay of aseg segmentation ventricles:
   aseg = subject.volume(subjects_dir, 'subject1', 'aseg');
   ventricle_mask = vol.mask.from.segmentation(aseg, c(4,14,15,43));
   vis.volume.on.surface(subjects_dir, 'subject1', ventricle_mask,
      volume_mode='voxels', measure=NULL, views=c('si'),
      volume_color='red');
} # }
```
