# Visualize volume slices with surface mesh contours overlaid in lightbox view.

Creates a lightbox view of 2D MRI slices with the cortical surface
boundary contours drawn on top. This is the fsbrain equivalent of what
FreeSurfer's `freeview` or `tkmedit` show — useful for quality
assessment of surface reconstruction against the underlying MRI volume.
The surface mesh is loaded from the subject's FreeSurfer directory and
its intersection with each displayed slice plane is computed and drawn
as colored contour lines.

## Usage

``` r
volvis.lb.with.surface(
  subjects_dir,
  subject_id,
  volume = "brain",
  surface = "white",
  hemi = "both",
  surface_color = "#FF0000",
  surface_lwd = 1,
  slices = -5,
  axis = 1L,
  per_row = 5L,
  per_col = NULL,
  border_geometry = "5x5",
  background_color = "#000000",
  silent = TRUE
)
```

## Arguments

- subjects_dir:

  character string, the FreeSurfer SUBJECTS_DIR.

- subject_id:

  character string, the subject identifier.

- volume:

  numeric 3D array or character string. Either a 3D brain volume (e.g.,
  from T1.mgz or brain.mgz), or the name of a volume file to load from
  the subject's `mri/` directory. Defaults to `"brain"`.

- surface:

  character string or vector of strings, the surface(s) to use for
  contour extraction. One or more of `"white"`, `"pial"`, or
  `"inflated"`. If a vector, contours for all surfaces are drawn.
  Defaults to `"white"`.

- hemi:

  character string, one of `'lh'`, `'rh'`, or `'both'`. Which hemisphere
  surface(s) to overlay. Defaults to `"both"`.

- surface_color:

  character string or character vector, the color(s) for the surface
  contour lines. If a single color, all surfaces and hemispheres use it.
  If a vector of length `length(surface)`, each surface gets its own
  color (both hemispheres use it). If a vector of length
  `2 * length(surface)`, colors are assigned as
  `(lh_surf1, rh_surf1, lh_surf2, rh_surf2, ...)`. Defaults to
  `"#FF0000"` (red).

- surface_lwd:

  numeric, line width for the contour lines (passed as `lwd` to
  [`segments`](https://rdrr.io/r/graphics/segments.html)). Defaults to
  1.

- slices:

  passed to
  [`volvis.lightbox`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.lightbox.md).
  A negative integer N means "use every Nth slice". A numeric vector
  gives explicit slice indices (1-based). Defaults to `-5` (every 5th
  slice).

- axis:

  integer, the slice axis. 1 = sagittal, 2 = coronal, 3 = axial (in
  volume CRS convention). Defaults to `1L`.

- per_row:

  integer, number of slice images per row in the lightbox grid. Defaults
  to `5L`.

- per_col:

  integer, number of rows. If `NULL`, computed from `per_row`. Defaults
  to `NULL`.

- border_geometry:

  character string, geometry for borders between tiles, passed to
  [`image_border`](https://docs.ropensci.org/magick/reference/composite.html).
  Defaults to `"5x5"`.

- background_color:

  character string, background color for borders and empty tiles.
  Defaults to `"#000000"`.

- silent:

  logical, whether to suppress messages. Defaults to `TRUE`.

## Value

a magick image instance containing the lightbox with surface contours
overlaid. Can be saved with
[`image_write`](https://docs.ropensci.org/magick/reference/editing.html)
or displayed interactively.

## See also

Other volume visualization:
[`vis.volume.on.surface()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.volume.on.surface.md),
[`volvis.lb()`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.lb.md),
[`volvis.lightbox()`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.lightbox.md),
[`volvis.slices.with.surface()`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.slices.with.surface.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_optional_data();
   subjects_dir <- fsbrain::get_optional_data_filepath("subjects_dir");

   # Axial slices with white surface contours in red:
   img <- volvis.lb.with.surface(subjects_dir, "subject1",
      volume="brain", surface="white", axis=3L,
      surface_color="#FF0000");
   magick::image_write(img, "~/fsbrain_qa_axial.png");

   # Coronal slices in green:
   img <- volvis.lb.with.surface(subjects_dir, "subject1",
      volume="brain", surface="white", axis=2L,
      surface_color="#00FF00");

   # Sagittal slices: left hemisphere red, right hemisphere blue:
   img <- volvis.lb.with.surface(subjects_dir, "subject1",
      volume="brain", surface="white", axis=1L,
      surface_color=c("#FF0000", "#0000FF"));

   # Single hemisphere, pial surface, every 5th slice:
   img <- volvis.lb.with.surface(subjects_dir, "subject1",
      volume="brain", surface="pial", hemi="lh",
      axis=3L, slices=-5, surface_color="#FF8800");

   # Both white and pial surfaces overlaid: white in red, pial in yellow:
   img <- volvis.lb.with.surface(subjects_dir, "subject1",
      volume="brain", surface=c("white", "pial"), axis=3L,
      surface_color=c("#FF0000", "#FFFF00"));

   # White (lh red, rh blue) + pial (lh green, rh orange):
   img <- volvis.lb.with.surface(subjects_dir, "subject1",
      volume="brain", surface=c("white", "pial"), axis=3L,
      surface_color=c("#FF0000", "#0000FF", "#00FF00", "#FF8800"));
} # }
```
