# Compute the transform between the voxel space of a subsampled volume and the original voxel space.

Subsampling a volume with a factor `f` keeps voxels 1, 1+f, 1+2f, ... of
the original volume, so the coordinate `i` of the subsampled volume
corresponds to the original 1-based R array index `i*f + (1-f)` (e.g.,
for `f = 2`: the subsampled voxels are the original voxels 1, 3, 5, ...
and they stay at 1, 3, 5 of the original volume). This function returns
the 4x4 affine matrix which implements this mapping. Note that it maps
to 1-based R array indices, so it has to be composed with
[`index2ras_tkr()`](https://dfsp-spirit.github.io/fsbrain/reference/index2ras_tkr.md)
(and not with
[`vox2ras_tkr()`](https://dfsp-spirit.github.io/fsbrain/reference/vox2ras_tkr.md),
which expects 0-based CRS indices) to transform subsampled shells to
surface RAS.

## Usage

``` r
volume.subsample.matrix(factor = 1L)
```

## Arguments

- factor:

  positive integer, the subsampling factor.

## Value

a 4x4 numerical matrix.
