# The FreeSurfer default ras2vox_tkr matrix.

Applying this matrix to a FreeSurfer surface RAS coordinate (from a
surface file like `lh.white`) will give you the voxel index (CRS) in a
conformed FreeSurfer volume. The returned matrix is the inverse of the
`vox2ras_tkr` matrix.

## Usage

``` r
ras2vox_tkr()
```

## Value

numeric 4x4 matrix, the FreeSurfer ras2vox_tkr matrix.

## See also

Other surface and volume coordinates:
[`vox2ras_tkr()`](https://dfsp-spirit.github.io/fsbrain/reference/vox2ras_tkr.md)

## Examples

``` r
   # Compute the FreeSurfer CRS voxel index of surface RAS coordinate (0.0, 0.0, 0.0):
   ras2vox_tkr() %*% c(0, 0, 0, 1);
#>      [,1]
#> [1,]  128
#> [2,]  128
#> [3,]  128
#> [4,]    1
   # Show that the voxel at surface RAS corrds (0.0, 0.0, 0.0) is the one with CRS (128, 128, 128):
   ras2vox_tkr() %*% c(0.0, 0.0, 0.0, 1);
#>      [,1]
#> [1,]  128
#> [2,]  128
#> [3,]  128
#> [4,]    1
```
