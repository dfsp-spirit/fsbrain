# Load a volume (from file or memory) and return the data along with the affine transformation.

Load a volume (from file or memory) and return the data along with the
affine transformation.

## Usage

``` r
vol.load(volume, default_dir = NULL, affine = NULL, vox2ras = "auto")
```

## Arguments

- volume:

  character string or named list. If a string, either a volume filepath
  or the name of a volume file without extension. If a named list, an
  in-memory volume with entries 'data' (numeric 3D or 4D array) and
  'affine' (4x4 numeric matrix mapping 0-based voxel indices to world
  coordinates).

- default_dir:

  character string or NULL, the directory used to resolve volumes
  specified as names without extension.

- affine:

  numeric 4x4 matrix or `NULL`. If given, it is used instead of the
  transformation matrix from the volume header. This is rarely needed,
  but can help if the header of a volume is missing or wrong. See
  `vox2ras` for the interpretation of a volume whose header does not
  match the surfaces.

- vox2ras:

  character string, how to compute the transformation from voxel indices
  to world coordinates for the volume: 'auto' (the default) uses
  [`vol.tkreg.affine`](https://dfsp-spirit.github.io/fsbrain/reference/vol.tkreg.affine.md)
  (the FreeSurfer tkregister convention, in which the surfaces are
  defined) for MGH/MGZ volumes, and the transformation matrix from the
  file header for NIfTI volumes. 'tkr' forces the tkregister convention,
  'header' forces the matrix from the volume header. This corresponds to
  the `--vox2ras-tkr` and `--vox2ras` options of `mri_vol2surf`. Ignored
  if `affine` is given.

## Value

named list with entries 'data', 'affine' and 'filepath' (see
[`vol.read.file.with.affine`](https://dfsp-spirit.github.io/fsbrain/reference/vol.read.file.with.affine.md)).
