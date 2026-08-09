# Show continuous 3D voxel/volume data as a lightbox, optionally with a background brain volume and colormap.

This function is the main way to visualize 3D volume images that contain
raw MRI scans or statistical results.

## Usage

``` r
volvis.lb(
  volume,
  background = NULL,
  colFn = viridis::viridis,
  colortable = NULL,
  no_act_source_value = 0,
  bbox_threshold = NULL,
  bbox_of_volume = TRUE,
  ...
)
```

## Arguments

- volume:

  numerical 3D array of per-voxel data, typically activation data, a raw
  MRI image, or a segmentation to show. Can also be a filename if the
  file can be loaded as such a volume with
  [`read.fs.volume`](https://rdrr.io/pkg/freesurferformats/man/read.fs.volume.html).

- background:

  numerical 3D array or 3D array of color strings, the background
  volume. Typically a raw brain volume. Dimensions and space must match
  those of the 'volume' for an array. Can also be a single file name as
  a character string. Can also be a single color name, like '#FEFEFE'
  but the string then must start with '#' (color names like 'red' are
  not allowed, they would be treated as file names). If a color string,
  be sure to use the `...` parameter to set the same color as
  `background_color` for the tiles.

- colFn:

  a colormap function, passed to `vol.overlay.colors.from.activation`
  and used as colormap for the 'volume' data. Pass NULL to derive
  gray-scale values from the raw data (only recommended with
  single-color backgrounds). Note that the colormap is not used for the
  the background data (if any), which will be shown in grayscale (unless
  it is a 3D array of color strings).

- colortable:

  optional, only makes sense for categorical 'volume' data like
  segmentations. If not NULL, a colortable as returned by
  [`read.fs.colortable`](https://rdrr.io/pkg/freesurferformats/man/read.fs.colortable.html),
  or a character string representing a path to a colortable file (like
  `"FREESURFER_HOME/FreeSurferColorLUT.txt"]`).

- no_act_source_value:

  numerical value, passed to `vol.overlay.colors.from.activation`.
  Specifies the value which is treated as transparent in the 'volume'
  parameter data (where you will see the background). If you need more
  control, e.g., you want to treat one or morge ranges of values as NA,
  you should load the 'volume' data first, modify it as needed, as pass
  it to this function afterwards. Set this parameter to `NULL` to
  disable it. Only for 'colFn', ignored if a 'colortable' is used.

- bbox_threshold:

  numerical scalar, passed on to `vol.merge`. If set, voxels with
  intensities smaller than this threshold will be dropped at the outside
  of the image. If `bbox_of_volume` parameter is `TRUE` (the default),
  this threshold applies to the 'volume', otherwise to the 'background'.
  Set to `NULL` to disable bounding box and show the full image.

- bbox_of_volume:

  logical, whether the bounding box is computed on the volume
  (foreground), which typically is what you want. Leave alone if in
  doubt.

- ...:

  extra parameters to be passed to
  [`volvis.lightbox`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.lightbox.md),
  can be used to select specific slices, set the `backgroud_color` for
  the border between and around the image tiles, etc.

## Note

This function should be preferred over manually calling
[`volvis.lightbox`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.lightbox.md).

## See also

Other volume visualization:
[`vis.volume.on.surface()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.volume.on.surface.md),
[`volvis.lb.with.surface()`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.lb.with.surface.md),
[`volvis.lightbox()`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.lightbox.md),
[`volvis.slices.with.surface()`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.slices.with.surface.md)

## Examples

``` r
if (FALSE) { # \dontrun{
volume = subject.volume(subjects_dir, subject_id, 'brain');
volvis.lb(volume);
volvis.lb("~/study1/subject1/mri/brain.mgz");
volvis.lb("~/study1/subject1/mri/brain.mgz", bbox_threshold = 1L);
volvis.lb("~/study1/subject1/mri/brain.mgz", background = "~/data/study1/subject1/mri/T1.mgz");
volvis.lb("~/study1/subject1/mri/brain.mgz", background = "#FEFEFE", background_color="#FEFEFE");
ct = file.path(find.freesurferhome(mustWork = T), "FreeSurferColorLUT.txt"); # ct = "color table"
volvis.lb("~/study1/subject1/mri/aseg.mgz", background="~/study1/subject1/mri/T1.mgz",
 colortable = ct, colFn=NULL, axis=2L);
volvis.lb("~/study1/subject1/mri/aseg.mgz", background = "~/study1/subject1/mri/T1.mgz",
 colortable = ct, colFn=NULL, bbox_threshold = 0);
} # }
```
