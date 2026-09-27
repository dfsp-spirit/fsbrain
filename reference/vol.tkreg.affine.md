# Transform a volume affine matrix into the FreeSurfer tkregister convention.

FreeSurfer stores brain surfaces (and all surface-based data) in the
*surface RAS* coordinate system, which is also called the *tkregister*
or *tkreg* space: the RAS coordinate (0, 0, 0) is at the center of the
field of view of the volume. The transformation from voxel indices to
this space is **not** the transformation stored in the header of the
volume: a volume can have a center-of-RAS offset (`cras`), which is
preserved by FreeSurfer in the header but ignored by the surface-based
tools (this is the difference between the `--vox2ras` and
`--vox2ras-tkr` options of `mri_vol2surf`).

This function takes the header transformation matrix of a volume and
returns the corresponding tkregister matrix, i.e., the same direction
cosines and voxel sizes, but with the translation replaced so that the
center of the volume is at RAS (0, 0, 0). For a conformed FreeSurfer
volume (256x256x256 voxels of 1 mm), the result is identical to
[`vox2ras_tkr`](https://dfsp-spirit.github.io/fsbrain/reference/vox2ras_tkr.md).

## Usage

``` r
vol.tkreg.affine(affine, dims)
```

## Arguments

- affine:

  numeric 4x4 matrix, the transformation matrix from the volume header
  (maps 0-based voxel indices to world coordinates).

- dims:

  integer vector of length 3 or more, the dimensions of the volume
  (`dim(volume_data)`).

## Value

numeric 4x4 matrix, the tkregister transformation matrix.
