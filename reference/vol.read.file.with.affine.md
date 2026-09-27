# Load a volume file and return the data along with the affine transformation.

Supports MGH/MGZ volumes (read with
[`read.fs.mgh`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.mgh.html))
and NIfTI v1 volumes, including oblique ones (read with
[`read.fs.volume.nii`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.volume.nii.html),
which requires the 'oro.nifti' package). The data array is returned in
the storage order of the file, i.e., it is *not* reoriented, and the
returned affine maps **0-based** voxel indices (column, row, slice) to
world coordinates. This is the convention used by the NIfTI `sform` and
by
[`mghheader.vox2ras`](https://dfsp-spirit.github.io/freesurferformats/reference/mghheader.vox2ras.html).

## Usage

``` r
vol.read.file.with.affine(filepath)
```

## Arguments

- filepath:

  character string, the path to an `.mgz`, `.mgh`, `.nii` or `.nii.gz`
  file.

## Value

named list with entries: 'data' (numeric array, 3D or 4D), 'affine' (4x4
numeric matrix), 'filepath' (character string), 'file_format' (character
string, either 'mgh' or 'nifti'), and 'frames' (integer, the number of
volumes in a 4D file).

## Note

If the header of an MGH/MGZ file does not contain a valid `vox2ras`
matrix (`ras_good_flag` is 0), the surface RAS convention
[`vox2ras_tkr`](https://dfsp-spirit.github.io/fsbrain/reference/vox2ras_tkr.md)
is used for conformed volumes (dimension 256x256x256) and a warning is
emitted. For NIfTI files without a valid sform/qform, an error is
raised, as the coordinate space is unknown in that case.
