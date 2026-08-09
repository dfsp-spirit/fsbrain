# Compute surface contour slice images (internal).

Internal helper that loads a volume and surface meshes, computes the
intersection contours of each surface with each slice plane, and draws
them onto individual magick slice images. Used by both
[`volvis.lb.with.surface`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.lb.with.surface.md)
(which grids the results) and
[`volvis.slices.with.surface`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.slices.with.surface.md)
(which exports them individually).

## Usage

``` r
compute.surface.contour.slices(
  subjects_dir,
  subject_id,
  volume = "brain",
  surface = "white",
  hemi = "both",
  surface_color = "#FF0000",
  surface_lwd = 1,
  slices = -5,
  axis = 1L,
  silent = TRUE
)
```

## Arguments

- subjects_dir:

  character string, the FreeSurfer SUBJECTS_DIR.

- subject_id:

  character string, the subject identifier.

- volume:

  numeric 3D array or character string. Either a 3D brain volume or the
  name of a volume file to load from the subject's `mri/` directory.
  Defaults to `"brain"`.

- surface:

  character string or vector of strings, the surface(s) to use for
  contour extraction.

- hemi:

  character string, one of `'lh'`, `'rh'`, or `'both'`.

- surface_color:

  character string or vector, color(s) for the contour lines.

- surface_lwd:

  numeric, line width for contours.

- slices:

  passed to
  [`volvis.lightbox`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.lightbox.md).
  A negative integer N means "use every Nth slice". A numeric vector
  gives explicit slice indices (1-based). Defaults to `-5`.

- axis:

  integer, the slice axis. 1 = sagittal, 2 = coronal, 3 = axial.
  Defaults to `1L`.

- silent:

  logical, whether to suppress messages.

## Value

named list with entries: `images` (list of magick images, one per
slice), `slice_indices` (integer vector of 1-based slice indices used),
`has_contour` (logical vector, TRUE if at least one contour segment was
drawn on the slice). Also includes metadata entries `axis`, `surfaces`,
`hemis`, `subject_id` for downstream use.
