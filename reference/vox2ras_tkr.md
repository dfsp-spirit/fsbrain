# The FreeSurfer default vox2ras_tkr matrix.

Applying this matrix to a FreeSurfer CRS index of a conformed volume
will give you the RAS coordinates of the voxel in surface coordinates,
i.e., in the coordinates used in surface file like `lh.white`. The
central voxel is 127,127,127 when using zero-based indices (or
128,128,128 when using one-based indices), meaning its surface RAS
coordinates are 0.0, 0.0, 0.0. The returned matrix is the inverse of the
`ras2vox_tkr` matrix.

## Usage

``` r
vox2ras_tkr()
```

## Value

numeric 4x4 matrix, the FreeSurfer vox2ras_tkr matrix.

## See also

Other surface and volume coordinates:
[`ras2vox_tkr()`](https://dfsp-spirit.github.io/fsbrain/reference/ras2vox_tkr.md)

## Examples

``` r
   # Compute surface RAS coordinate of voxel with CRS (0L, 0L, 0L):
   vox2ras_tkr() %*% c(0, 0, 0, 1);
#>      [,1]
#> [1,]  128
#> [2,] -128
#> [3,]  128
#> [4,]    1
   # Show that voxel with CRS (128,128,128) is at the
   #  origin (0.0, 0.0, 0.0) of the surface RAS coordinate system:
   (vox2ras_tkr() %*% c(128, 128, 128, 1))[1:3];
#> [1] 0 0 0
```
