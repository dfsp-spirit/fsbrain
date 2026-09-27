# Visualize white matter tracts (streamlines) on the cortical surface.

Draws white matter streamlines (tractography data) as lines, with an
optional semi-transparent brain surface as context. The streamlines can
come from a tract file (TRK or TCK format), from a directory of such
files (a tract atlas with one file per bundle, e.g. the XTRACT atlas,
see
[`download_xtract_tracts`](https://dfsp-spirit.github.io/fsbrain/reference/download_xtract_tracts.md)),
or from in-memory data. This is the fsbrain equivalent of the
`plot_tracts` function of the Python package `yabplot`.

## Usage

``` r
vis.tracts(
  tracts,
  bundle_values = NULL,
  subjects_dir = NULL,
  template_id = "fs_LR_32",
  context = list(surface = "midthickness", alpha = 0.08, color = "#B0B0B0"),
  coords = "ras",
  transform_matrix = NULL,
  max_tracks = Inf,
  skip_tracks = 0L,
  bbox = NULL,
  tract_color = NULL,
  tract_makecmap_options = mkco.seq(),
  tract_width = 1,
  tract_width_range = NULL,
  tract_width_scale = c("none", "value"),
  color_by_orientation = FALSE,
  views = c("sd_lateral_lh", "sd_medial_lh", "sd_lateral_rh", "sd_medial_rh"),
  rgloptions = rglo(),
  rglactions = list(),
  style = "default",
  draw_colorbar = FALSE,
  silent = FALSE
)
```

## Arguments

- tracts:

  the tracts to draw. One of: a character string, the path to a tract
  file (TRK or TCK format) or to a directory containing one file per
  bundle (a tract atlas); a vector of tract file paths; an `fs.tracts`
  instance (`freesurferformats`); an (n, 3) matrix of coordinates (a
  single streamline); or a list of such objects. A list of (n, 3)
  matrices is interpreted as a single bundle (one tractogram), a list of
  `fs.tracts` instances as one bundle per entry. For the atlas case, the
  bundle names are the file names without extension.

- bundle_values:

  numeric vector or NULL, one value per bundle, in the order of the
  bundles (or named with the bundle names). The values are mapped to
  colors, see the details.

- subjects_dir:

  character string or NULL, the subjects directory that contains the
  template. If NULL, the standard fsbrain locations are searched, see
  [`resolve.template.subjects.dir`](https://dfsp-spirit.github.io/fsbrain/reference/resolve.template.subjects.dir.md).

- template_id:

  character string, the template or subject to draw the context surface
  for. Defaults to 'fs_LR_32', the HCP-style surface space that is
  widely used for tractography data. Note that the template meshes are
  not part of this package, see
  [`download_fs_LR_32_meshes`](https://dfsp-spirit.github.io/fsbrain/reference/download_fs_LR_32_meshes.md).

- context:

  named list or NULL, the semi-transparent brain surface drawn around
  the tracts. Entries are 'surface' (the surface name, default
  'midthickness'), 'alpha' (the transparency, default 0.08) and 'color'
  (default '#B0B0B0'). Set to NULL to draw the tracts without any
  context surface.

- coords:

  character string or NULL, the coordinate system of TRK files, see
  [`read.tract.bundles`](https://dfsp-spirit.github.io/fsbrain/reference/read.tract.bundles.md).
  Ignored for in-memory data.

- transform_matrix:

  a 4x4 numeric matrix or NULL, a transformation that is applied to all
  tract coordinates after reading them. Note that tract data and
  template surfaces have to be in the same space for the overlay to be
  correct: the XTRACT atlas, for example, is defined in MNI152 space,
  while the fs_LR_32 and fsaverage templates use an fsaverage-like
  (MNI305) space, which is close but not identical, and both are close
  enough for the overlay to look right without a transformation. Use
  this parameter if you have an accurate transformation, e.g. from a
  registration of the template to MNI152 space.

- max_tracks:

  numeric, the maximum number of streamlines to read per file, see
  [`read.tract.bundles`](https://dfsp-spirit.github.io/fsbrain/reference/read.tract.bundles.md).
  This is the way to plot a subset of a whole-brain tractogram, which
  can contain millions of streamlines and cannot be drawn (or held in
  memory) as a whole.

- skip_tracks:

  integer, the number of streamlines to skip per file, see
  [`read.tract.bundles`](https://dfsp-spirit.github.io/fsbrain/reference/read.tract.bundles.md).

- bbox:

  numeric vector of length 6 or NULL, a bounding box to select the
  streamlines that pass through a region, see
  [`read.tract.bundles`](https://dfsp-spirit.github.io/fsbrain/reference/read.tract.bundles.md).

- tract_color:

  vector of hex color strings, the color(s) of the tracts. Either a
  single color which is used for all bundles, or one color per bundle.
  Ignored if `bundle_values` is given. If both `tract_color` and
  `bundle_values` are NULL and `color_by_orientation` is FALSE, all
  tracts are drawn in a single default color.

- tract_makecmap_options:

  named list of colormap options, see
  [`mkco.seq`](https://dfsp-spirit.github.io/fsbrain/reference/mkco.seq.md),
  used to map `bundle_values` to colors.

- tract_width:

  a single positive number, the line width in pixels.

- tract_width_range:

  numeric vector of length 2 or NULL, the range of the line widths used
  when the widths are scaled by the bundle values, see
  `tract_width_scale`. Defaults to `c(0.5, 1.5) * tract_width`.

- tract_width_scale:

  character string, either 'none' (all lines have the same width, the
  default) or 'value' (the line width encodes the bundle values, which
  requires `bundle_values` to be given).

- color_by_orientation:

  logical, whether to color the segments by their direction instead of
  by bundle, see the details. The colors then do not encode the bundle
  values, so no colorbar is drawn.

- views:

  vector of character strings, the views to render, see
  [`brainviews`](https://dfsp-spirit.github.io/fsbrain/reference/brainviews.md).
  Pass `NULL` to only compute the renderables without rendering anything
  (e.g. to pass them to
  [`export`](https://dfsp-spirit.github.io/fsbrain/reference/export.md)).

- rgloptions:

  named list, options for
  [`rgl::par3d()`](https://dmurdoch.github.io/rgl/dev/reference/par3d.html),
  see [`rglo`](https://dfsp-spirit.github.io/fsbrain/reference/rglo.md).

- rglactions:

  named list, actions to perform, see
  [`rglactions`](https://dfsp-spirit.github.io/fsbrain/reference/rglactions.md).

- style:

  character string, the rendering style for the tracts, see
  [`material3d`](https://dmurdoch.github.io/rgl/dev/reference/material.html).
  The context surface is always rendered with its own fixed style, so
  that the requested transparency is used.

- draw_colorbar:

  logical or one of the character strings 'vertical' or 'horizontal',
  whether to draw a colorbar for the bundle values. Note that the
  headless scimesh renderer backend does not support colorbars in this
  function, use
  [`export`](https://dfsp-spirit.github.io/fsbrain/reference/export.md)
  instead (see the return value).

- silent:

  logical, whether to suppress the progress messages.

## Value

invisible named list of renderables: the entry `tracts` are the lines
(an `fs.coloredpaths` instance), and `context_lh` and `context_rh` the
context surfaces (if `context` is not NULL). The list can be passed to
[`export`](https://dfsp-spirit.github.io/fsbrain/reference/export.md).

## Details

The streamlines are drawn with the line renderable of the connectome
code, see
[`fs.coloredpaths`](https://dfsp-spirit.github.io/fsbrain/reference/fs.coloredpaths.md),
so the result is a regular fsbrain renderable list which works with both
renderer backends (rgl and the headless scimesh renderer) and can be
passed to
[`export`](https://dfsp-spirit.github.io/fsbrain/reference/export.md) to
create publication quality figures with a colorbar.

The colors can be defined in three ways: (1) as one value per bundle via
`bundle_values` (e.g. a tractometry measure like the mean fractional
anisotropy of each bundle), which is mapped to colors with
`tract_makecmap_options` and produces a colorbar; (2) as a single color
for all bundles via `tract_color`; or (3) per segment, based on the
direction of the segment, via `color_by_orientation`, which is the
classic DTI look (red/green/blue for
left-right/anterior-posterior/superior-inferior).

## See also

Other tracts functions:
[`download_xtract_tracts()`](https://dfsp-spirit.github.io/fsbrain/reference/download_xtract_tracts.md),
[`read.tract.bundles()`](https://dfsp-spirit.github.io/fsbrain/reference/read.tract.bundles.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  # Download the XTRACT tract atlas (see the function documentation):
  download_xtract_tracts("xtract_tiny");

  atlas_dir = file.path(get_optional_data_filepath("tracts"), "xtract_tiny");
  bundles = read.tract.bundles(atlas_dir);

  # One color per bundle, colors follow a colormap, with a colorbar in the figure:
  values = seq(0.2, 0.8, length.out = length(bundles));
  names(values) = names(bundles);
  tracts = vis.tracts(bundles, bundle_values = values,
    views = c("sd_lateral_lh", "sd_lateral_rh"));

  # Or a publication quality figure with 3 views and a colorbar:
  export(tracts, view_angles = c("sd_lateral_lh", "sd_dorsal", "sd_rostral"),
    draw_colorbar = "horizontal", colorbar_legend = "Mean FA",
    output_img = "tracts.png");

  # Draw a whole-brain tractogram from MRtrix (a subset, it is huge):
  vis.tracts("~/data/sub-01_streamlines.tck.gz", max_tracks = 20000);
} # }
```
