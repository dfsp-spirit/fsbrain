# The affine matrix that maps 1-based R array indices to surface RAS.

Meshes and coordinates that were computed from a volume *array* in R are
expressed in **1-based R array indices**: the first voxel of the volume
is at index 1, the second at index 2, and so on. This is the convention
of the iso-surfaces returned by
[`shell.extract.mesh`](https://dfsp-spirit.github.io/fsbrain/reference/shell.extract.mesh.md)
and
[`volvis.contour`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.contour.md),
and of the indices returned by `which(volume != 0, arr.ind = TRUE)`.

The FreeSurfer
[`vox2ras_tkr`](https://dfsp-spirit.github.io/fsbrain/reference/vox2ras_tkr.md)
matrix, in contrast, expects **0-based CRS** (column, row, slice)
indices: the first voxel of the volume is at CRS (0, 0, 0), and (as the
name says) the *voxel* index is mapped to RAS. Feeding 1-based R indices
to
[`vox2ras_tkr()`](https://dfsp-spirit.github.io/fsbrain/reference/vox2ras_tkr.md)
shifts the result by one voxel (1 mm for a conformed volume), which is
why volume data seemed to be slightly offset from the brain surface it
was rendered with.

This function returns the matrix which maps 1-based R array indices to
surface RAS, i.e., `vox2ras_tkr() %*% translation.matrix(-1, -1, -1)`.
It is used internally by the volume visualization functions, and can be
used to transform the meshes returned by
[`volvis.contour`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.contour.md)
(or
[`misc3d::contour3d`](https://rdrr.io/pkg/misc3d/man/contour3d.html)) so
that they are aligned with surface renderings of the same subject.

## Usage

``` r
index2ras_tkr()
```

## Value

numeric 4x4 matrix, the mapping from 1-based R array indices to surface
RAS.

## See also

Other surface and volume coordinates:
[`ras2vox_tkr()`](https://dfsp-spirit.github.io/fsbrain/reference/ras2vox_tkr.md),
[`vox2ras_tkr()`](https://dfsp-spirit.github.io/fsbrain/reference/vox2ras_tkr.md)

## Examples

``` r
   # The first voxel of a conformed volume (R index 1) is at CRS (0, 0, 0):
   (index2ras_tkr() %*% c(1, 1, 1, 1))[1:3];
#> [1]  128 -128  128
   # which is the position reported by the vox2ras_tkr matrix for CRS (0, 0, 0):
   (vox2ras_tkr() %*% c(0, 0, 0, 1))[1:3];
#> [1]  128 -128  128
```
