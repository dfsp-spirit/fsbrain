# Project a volume onto the cortical surface of a subject using its own (native) space.

Samples a brain volume at the positions of the cortical surface vertices
of a subject and returns the resulting per-vertex values. This is the
equivalent of the FreeSurfer command line tool `mri_vol2surf` (or of the
`project_vol2surf` function of the Python package `yabplot`),
implemented in R. The volume and the surface must be defined in the same
coordinate space, which is the case for the volumes in the `mri`
directory of a subject (e.g., `brain.mgz`) and the surfaces in its
`surf` directory (e.g., `lh.white`) – both are in the subject's native
space.

The volume is sampled at the vertex positions using trilinear
interpolation (default) or nearest neighbor interpolation. Vertices
which fall outside the volume (e.g., medial wall vertices at the bottom
of a `brainmask` volume, or vertices outside a truncated field of view)
get the value NA by default, and a warning is emitted. Use
`clamp = TRUE` to instead use the value of the closest voxel at the
volume border, and `check_fov = FALSE` to suppress the check.

To project a group-level statistical map (e.g., in MNI space) onto a
template like `fsaverage`, see
[`template.vol2surf`](https://dfsp-spirit.github.io/fsbrain/reference/template.vol2surf.md).

## Usage

``` r
subject.vol2surf(
  subjects_dir,
  subject_id,
  volume,
  surface = "white",
  hemi = "both",
  surface_frac = NULL,
  frac_surface = "pial",
  interpolation = "trilinear",
  frame = 1L,
  cortex_only = FALSE,
  clamp = FALSE,
  check_fov = TRUE,
  affine = NULL,
  vox2ras = "auto"
)
```

## Arguments

- subjects_dir:

  character string. The FreeSurfer `SUBJECTS_DIR`, i.e., a directory
  containing the data for all your subjects, each in a subdir named
  after the subject identifier.

- subject_id:

  character string. The subject identifier.

- volume:

  character string or named list. A volume filepath, the name of a
  volume file in the `mri` directory of the subject without file
  extension (e.g., `brain` or `aseg`), or an in-memory volume given as a
  named list with entries 'data' (numeric 3D or 4D array) and 'affine'
  (4x4 numeric matrix mapping **0-based** voxel indices to world
  coordinates). Examples for the latter are
  [`read.fs.mgh`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.mgh.html)
  (use `with_header = TRUE` and pass
  `list(data = vol$data, affine = vol$header$vox2ras_matrix)`) and
  `nibabel` in Python.

- surface:

  character string, the name of the surface to sample. Examples:
  'white', 'pial', 'midthickness'. The corresponding `surf/?h.<surface>`
  files must exist for the subject.

- hemi:

  character string, one of 'lh', 'rh' or 'both'. The hemisphere to
  project to.

- surface_frac:

  numeric scalar in range 0..1, or `NULL` (the default). If given, the
  volume is not sampled at the vertices of `surface`, but at the
  vertices of the surface which lies at the given fraction on the line
  between the corresponding vertices of `surface` (fraction 0) and
  `frac_surface` (fraction 1). E.g., with `surface = "white"`,
  `frac_surface = "pial"` and `surface_frac = 0.5`, the mid-cortical
  surface is used. This is what the `--surf-frac` option of
  `mri_vol2surf` does. Note that no surface normals are required, the
  interpolation is purely geometric.

- frac_surface:

  character string, the second surface for `surface_frac`. Ignored if
  `surface_frac` is `NULL`. Defaults to 'pial'.

- interpolation:

  character string, one of 'trilinear' (the default, an alias 'linear'
  is accepted) or 'nearest'. Trilinear interpolation is suitable for
  continuous data like thickness or t-statistics, while nearest neighbor
  is what you want for discrete data like atlas labels, segmentation
  indices or p-values (it never averages the values of neighboring
  voxels).

- frame:

  positive integer scalar, the index of the volume ('frame') to use if
  `volume` contains more than one. Defaults to 1. Ignored for 3D
  volumes.

- cortex_only:

  logical, whether to set the values of all vertices which are *not*
  part of the cortex (as defined by the label file
  `label/?h.cortex.label`) to NA. This masks the medial wall. Defaults
  to FALSE.

- clamp:

  logical. How to handle surface vertices which are outside the volume:
  if FALSE (the default), their value is set to NA; if TRUE, the value
  of the closest voxel at the volume border is used instead (edge
  replication, like the `mode = 'nearest'` parameter of
  `scipy.ndimage.map_coordinates`). See also `check_fov`.

- check_fov:

  logical, whether to check for surface vertices which are outside the
  volume and emit a warning listing them. Defaults to TRUE. The check is
  very cheap (a few comparisons per vertex), the parameter exists mainly
  so that you can silence the warning when you know that a large part of
  the surface is outside the volume on purpose.

- affine:

  numeric 4x4 matrix or `NULL` (the default). The matrix that maps
  **0-based** voxel indices to world coordinates, i.e., the coordinate
  space in which the surface vertices are defined. By default this is
  derived from the volume (see `vox2ras`). Use this parameter only if
  you want to override the transformation completely.

- vox2ras:

  character string, how the transformation from voxel indices to the
  coordinate space of the surface is obtained. One of 'auto' (the
  default), 'tkr' or 'header'. For MGH/MGZ volumes, 'auto' uses the
  FreeSurfer *tkregister* convention, in which the brain surfaces are
  defined: the direction cosines and voxel sizes from the volume header,
  but with the origin moved to the center of the volume (see
  [`vox2ras_tkr`](https://dfsp-spirit.github.io/fsbrain/reference/vox2ras_tkr.md)
  and
  [`vol.tkreg.affine`](https://dfsp-spirit.github.io/fsbrain/reference/vol.tkreg.affine.md)).
  This matches what the FreeSurfer tools do (it is the difference
  between the `--vox2ras-tkr` and the `--vox2ras` option of
  `mri_vol2surf`), and it matters for volumes whose header carries a
  non-zero center-of-RAS offset (`cras`) – such volumes are common,
  e.g., when the data was imported from DICOM. For NIfTI volumes, 'auto'
  uses the transformation matrix from the file header (`sform`/`qform`),
  as there is no tkregister convention for NIfTI files. Use 'header' or
  'tkr' to force one of the two interpretations. Ignored if `affine` is
  given.

## Value

a numerical vector of per-vertex values, one value per surface vertex,
or a hemilist (named list with entries `lh` and `rh`) of such vectors if
`hemi` is 'both'. Note that the values are in the order of the vertices
of the *sampled* surface, which is the order used by the surface file,
so they can be used directly with functions like
[`vis.subject.morph.native`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.morph.native.md).

## Note

Trilinear interpolation is used by default, while the FreeSurfer tool
`mri_vol2surf` defaults to nearest neighbor interpolation: pass
`interpolation = "nearest"` to reproduce its output exactly. Note also
that the affine matrix of a NIfTI file is converted into an MGH-style
header by the reader, which stores the direction cosines with limited
precision: for volumes with an oblique (rotated) orientation the
transformation can differ from the value in the file by about 1e-07, and
sheared transformations are not supported (the reader warns about this).

## See also

Other volume to surface projection functions:
[`template.vol2surf()`](https://dfsp-spirit.github.io/fsbrain/reference/template.vol2surf.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_optional_data();
   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
   # Project the brain volume of subject1 onto the white surface:
   brain_morph = subject.vol2surf(subjects_dir, "subject1", volume = "brain", surface = "white");
   # Project the same volume onto the mid-cortical surface (between white and pial):
   mid_morph = subject.vol2surf(subjects_dir, "subject1", volume = "brain",
       surface = "white", frac_surface = "pial", surface_frac = 0.5);
   # Visualize the result on the surface:
   vis.subject.morph.native(subjects_dir, "subject1", morph_data = brain_morph);
} # }
```
