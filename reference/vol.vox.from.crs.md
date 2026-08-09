# Compute R voxel index for FreeSurfer CRS voxel index.

Performs a vox2vos transform from FreeSurfer to R indices.

## Usage

``` r
vol.vox.from.crs(fs_crs, add_affine = FALSE)
```

## Arguments

- fs_crs:

  integer vector of length 3, Freesurfer indices for column, row, and
  slice (CRS).

- add_affine:

  logical, whether to add 1 to the output vector as the 4th value

## Value

the R indices into the volume data for the given FreeSurfer CRS indices

## Examples

``` r
   # Get voxel intensity data on the command line, based
   #  on the FreeSUrfer (zero-based) CRS voxel indices:
   #  `mri_info --voxel 127 100 100 ~/data/tim_only/tim/mri/brain.mgz`
   # (the result is: 106.0)
   #
   # That should be identical to:
   # our_crs = vol.vox.from.crs(c(127,100,100), add_affine = FALSE);
   # brain$data[our_crs[1], our_crs[2], our_crs[3]];   # gives 106
```
