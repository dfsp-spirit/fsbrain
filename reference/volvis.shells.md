# Visualize a volume as nested, semi-transparent iso-surface shells.

Visualize the iso-surfaces (*shells*) of a volume at several levels as
nested, semi-transparent meshes, like the nested contour lines of a
topographic map. This is useful to show the shape and the internal
structure of a volume at the same time, e.g., for a statistical map: the
outer shells (low levels) are rendered more transparently than the inner
ones (high levels), so that the inner structure remains visible. The
volume is extracted in voxel space and transformed to surface RAS with
the
[`index2ras_tkr`](https://dfsp-spirit.github.io/fsbrain/reference/index2ras_tkr.md)
matrix, so the result is spatially aligned with surface renderings of
the same subject (see
[`vis.subject.morph.native`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.morph.native.md)).
To combine the shells with a surface rendering, use the `rglactions`
parameter of the surface function (key 'no_vis') to keep the rendering
window open, or call this function with `views = NULL` and render the
returned meshes yourself.

## Usage

``` r
volvis.shells(
  volume,
  levels = NULL,
  num_levels = 4L,
  level_type = "quantile",
  level_range = c(0.2, 0.95),
  frame = 1L,
  colors = NULL,
  alphas = NULL,
  palette = "grey_context",
  alpha_range = NULL,
  smoothing = 1L,
  downsample = 1L,
  cut_away = NULL,
  cut_fraction = 0.5,
  backend = "auto",
  views = c("sd_lateral_lh", "sd_medial_lh", "sd_lateral_rh", "sd_medial_rh"),
  rgloptions = rglo(),
  rglactions = list(),
  silent = FALSE
)
```

## Arguments

- volume:

  a 3D numerical array (or an `fs.volume` instance), the volume to
  visualize. Values 0 and `NA` are treated as background when the levels
  are computed automatically, and the shells are the iso-surfaces at the
  computed (or given) levels.

- levels:

  numerical vector or `NULL`. The iso-levels at which to extract shells,
  in the units of the volume data. Must be ascending (the first level is
  the outermost shell). If `NULL` (the default), the levels are computed
  automatically, see `num_levels` and `level_type`.

- num_levels:

  positive integer, the number of shells to create. Ignored if `levels`
  is given. Defaults to 5.

- level_type:

  character string, how to compute the levels if `levels` is `NULL`. One
  of 'quantile' (the default) or 'linear'. For 'quantile', each shell
  contains the same fraction of the foreground voxels, for 'linear', the
  levels are equally spaced within the range given by `level_range`. In
  both cases only voxels with finite, non-zero values are considered.

- level_range:

  numerical vector of length 2, the range from which the automatic
  levels are taken. For `level_type = 'quantile'`, this is interpreted
  as quantiles (values between 0 and 1) of the foreground values, for
  `level_type = 'linear'` as fractions of the range of the foreground
  values. Defaults to `c(0.2, 0.95)`, i.e., the innermost shell is at
  the 95th percentile of the voxel values, so that a few extreme voxels
  do not dominate the visualization.

- frame:

  positive integer, the frame (4th dimension) to use for a 4D volume.
  Defaults to 1.

- colors:

  character vector of colors, one per shell (outermost shell first).
  Defaults to `NULL`, in which case the colors are taken from the
  `palette`.

- alphas:

  numerical vector of alpha values, one per shell (outermost shell
  first), values between 0 and 1. Defaults to `NULL`, in which case the
  alpha values are taken from the `palette`.

- palette:

  character string, the color and transparency recipe used for the
  shells if `colors` and/or `alphas` are not given, one of
  'grey_context' (the default), 'sequential' or 'viridis'. See
  [`shell.palette`](https://dfsp-spirit.github.io/fsbrain/reference/shell.palette.md)
  for the details.

- alpha_range:

  numerical vector of length 2 or `NULL`, the alpha value of the
  outermost and of the innermost shell. If `NULL` (the default), the
  range defined by the `palette` is used. Ignored if `alphas` is given.

- smoothing:

  non-negative integer, the number of 3x3x3 box blur passes applied to
  the volume before the shells are extracted. This removes the staircase
  artifacts of the marching cubes algorithm and leads to much nicer
  surfaces. Defaults to 1. Set to 0 to disable and extract the shells
  from the raw volume.

- downsample:

  positive integer, a factor by which the volume is subsampled before
  the shells are extracted. Values larger than 1 reduce the number of
  triangles (and thus the rendering time) considerably, at the cost of a
  less detailed surface. The coordinates of the subsampled shells are
  corrected, so the result is still aligned with the original volume.
  Defaults to 1. Note that the volume is smoothed before subsampling if
  `smoothing` is larger than 0, otherwise the shells are prone to
  aliasing artifacts.

- cut_away:

  character string or `NULL`. If not `NULL`, the volume is cut open
  along one of the six anatomical directions, so that one can look
  inside: the part of the volume in the given direction is removed.
  Valid values are 'left', 'right', 'posterior', 'anterior', 'inferior'
  and 'superior' (in FreeSurfer RAS convention, i.e., +x is right, +y is
  anterior, +z is superior). The caps of the cut are not closed, so the
  shells are open at the cut plane. Defaults to `NULL`, i.e., no cut.

- cut_fraction:

  numerical value between 0 and 1, the position of the cut along the
  axis given by `cut_away`, as a fraction of the bounding box of the
  shells. Defaults to 0.5, i.e., the volume is cut in the middle.

- backend:

  character string, the backend used to extract the iso-surfaces. One of
  'auto' (the default: use the `Rvcg` package if it is installed, and
  fall back to `misc3d` otherwise), 'Rvcg' or 'misc3d'. `Rvcg` is the
  recommended backend: it is faster and returns vertex-welded meshes
  (i.e., meshes that need much less memory) with normals. Note that at
  least one of the two optional packages must be installed.

- views:

  list of character strings, the views to visualize. Available views are
  `'sd_<angle>'` (a static view, see
  [`vis.subject.morph.native`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.morph.native.md)),
  't4'/'t9' (a 2x2 or 3x3 lightbox of views) and 'si'/'sr' (single
  interactive / rotating, requires the rgl renderer backend). Defaults
  to the 4 standard lateral and medial views. Pass `NULL` to skip the
  rendering entirely and only compute the meshes (which are also
  returned).

- rgloptions:

  option list passed to
  [`par3d`](https://dmurdoch.github.io/rgl/dev/reference/par3d.html).
  Defaults to the package default, see
  [`rglo`](https://dfsp-spirit.github.io/fsbrain/reference/rglo.md).

- rglactions:

  named list, passed to the visualization functions, see
  [`rglactions`](https://dfsp-spirit.github.io/fsbrain/reference/rglactions.md).
  The key 'no_vis' is useful to prepare the scene and add other data to
  it later. Note that saving the rendered scene to an image file is done
  with
  [`export`](https://dfsp-spirit.github.io/fsbrain/reference/export.md),
  which takes the return value of this function and supports merging
  several views into one image.

- silent:

  logical, whether to suppress the console messages which report the
  levels and the backend used. Defaults to `FALSE`.

## Value

the list of `fs.coloredmesh` instances, one per shell (from the
outermost to the innermost), invisibly. Note that the colorbar of the
rendering functions is not used here, the color of a shell encodes its
level, which is reported in the console unless `silent` is `TRUE`.

## Note

Both renderers are used with the mesh's own style (see
[`get.rglstyle`](https://dfsp-spirit.github.io/fsbrain/reference/get.rglstyle.md)),
which sets the alpha value of the shell. Note that backfaces are
rendered for the shells (`back = 'filled'`): if they were culled, the
rgl renderer would composite the nested transparent shells in the wrong
order (the innermost shell would end up on top of the outer ones).

## See also

[`volvis.contour`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.contour.md)
for a single iso-surface,
[`volvis.voxels`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.voxels.md)
for a voxel-based rendering of a volume, and
[`vis.volume.on.surface`](https://dfsp-spirit.github.io/fsbrain/reference/vis.volume.on.surface.md)
to combine a volume with a surface rendering.

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_optional_data();
   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
   brain = subject.volume(subjects_dir, 'subject1', 'brain');
   # Show the 5 inner shells of the brain, cut open from the right:
   shells = volvis.shells(brain, num_levels = 5, downsample = 2, cut_away = 'right', views = NULL);
   # Combine the 4 standard views into a single image:
   view_angles = c('sd_lateral_lh', 'sd_medial_lh', 'sd_lateral_rh', 'sd_medial_rh');
   fsbrain::export(shells, view_angles = view_angles, output_img = 'shells.png');
} # }
```
