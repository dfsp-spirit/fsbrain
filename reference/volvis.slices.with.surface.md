# Export individual volume slices with surface contours to image files.

Creates individual 2D slice images with surface contour overlays and
exports them to separate files — ideal for browsing through slices one
by one. This is the single-slice export counterpart of
[`volvis.lb.with.surface`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.lb.with.surface.md),
which arranges slice images in a lightbox grid. Supports optional slice
index labelling and automatic skipping of empty slices (i.e., slices
where no surface contour intersects the plane).

## Usage

``` r
volvis.slices.with.surface(
  subjects_dir,
  subject_id,
  volume = "brain",
  surface = "white",
  hemi = "both",
  surface_color = "#FF0000",
  surface_lwd = 1,
  slices = -5,
  axis = 1L,
  silent = TRUE,
  output_dir = ".",
  output_prefix = NULL,
  image_format = "png",
  label_slices = FALSE,
  label_color = "white",
  label_size = 20,
  label_gravity = "northwest",
  skip_empty = FALSE
)
```

## Arguments

- subjects_dir:

  character string, the FreeSurfer SUBJECTS_DIR.

- subject_id:

  character string, the subject identifier.

- volume:

  numeric 3D array or character string. Either a 3D brain volume, or the
  name of a volume file to load from the subject's `mri/` directory.
  Defaults to `"brain"`.

- surface:

  character string or vector of strings, the surface(s) to use for
  contour extraction. One or more of `"white"`, `"pial"`, or
  `"inflated"`. Defaults to `"white"`.

- hemi:

  character string, one of `'lh'`, `'rh'`, or `'both'`. Which hemisphere
  surface(s) to overlay. Defaults to `"both"`.

- surface_color:

  character string or character vector, the color(s) for the surface
  contour lines. See
  [`volvis.lb.with.surface`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.lb.with.surface.md)
  for the color assignment rules. Defaults to `"#FF0000"` (red).

- surface_lwd:

  numeric, line width for the contour lines. Defaults to 1.

- slices:

  passed to
  [`volvis.lightbox`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.lightbox.md).
  A negative integer N means "use every Nth slice". A numeric vector
  gives explicit slice indices (1-based). Defaults to `-5`.

- axis:

  integer, the slice axis. 1 = sagittal, 2 = coronal, 3 = axial (in
  volume CRS convention). Defaults to `1L`.

- silent:

  logical, whether to suppress messages. Defaults to `TRUE`.

- output_dir:

  character string, directory in which to write the slice image files.
  Created if it does not exist. Defaults to `"."` (current working
  directory).

- output_prefix:

  character string, prefix for output filenames. If `NULL`, auto-derived
  as `"<subject_id>"`. Defaults to `NULL`.

- image_format:

  character string, image format for output files (e.g., `"png"`,
  `"jpg"`). Passed to
  [`image_write`](https://docs.ropensci.org/magick/reference/editing.html).
  Defaults to `"png"`.

- label_slices:

  logical, whether to annotate each slice image with its slice index
  (e.g., "slice 42") in the top-left corner. Uses
  [`image_annotate`](https://docs.ropensci.org/magick/reference/painting.html).
  Defaults to `FALSE`.

- label_color:

  character string, color for the slice label text. Defaults to
  `"white"`.

- label_size:

  integer, font size for the slice label. Defaults to 20.

- label_gravity:

  character string, gravity for label placement, passed to
  [`image_annotate`](https://docs.ropensci.org/magick/reference/painting.html).
  Defaults to `"northwest"`.

- skip_empty:

  logical, whether to skip slices where no surface contour intersects
  the slice plane. Defaults to `FALSE`.

## Value

invisible character vector of file paths that were written.

## See also

Other volume visualization:
[`vis.volume.on.surface()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.volume.on.surface.md),
[`volvis.lb()`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.lb.md),
[`volvis.lb.with.surface()`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.lb.with.surface.md),
[`volvis.lightbox()`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.lightbox.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_optional_data();
   subjects_dir <- fsbrain::get_optional_data_filepath("subjects_dir");

   # Export all sagittal slices with white surface contours:
   volvis.slices.with.surface(subjects_dir, "subject1",
      volume="brain", surface="white", axis=1L,
      output_dir="~/qa_slices");

   # Export with slice labels, skipping empty slices:
   volvis.slices.with.surface(subjects_dir, "subject1",
      volume="brain", surface="white", axis=3L,
      output_dir="~/qa_slices", label_slices=TRUE,
      skip_empty=TRUE);

   # Both white and pial surfaces, different colors:
   volvis.slices.with.surface(subjects_dir, "subject1",
      volume="brain", surface=c("white","pial"), axis=3L,
      surface_color=c("#FF0000","#FFFF00"),
      output_dir="~/qa_slices");
} # }
```
