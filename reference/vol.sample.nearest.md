# Sample a 3D volume at the given voxel coordinates using nearest neighbor interpolation.

Sample a 3D volume at the given voxel coordinates using nearest neighbor
interpolation.

## Usage

``` r
vol.sample.nearest(volume_data, coords)
```

## Arguments

- volume_data:

  numeric 3D array, the volume data.

- coords:

  numeric matrix with 3 columns and one row per query point, the
  **0-based** voxel coordinates. Coordinates must be within the volume.

## Value

numeric vector, one value per query point.

## Note

A coordinate which is exactly between two voxels is resolved towards the
larger voxel index, i.e., `floor(coord + 0.5)` is used. This matches the
behavior of `scipy.ndimage.map_coordinates(..., order = 0)`. Note that
R's `round` uses banker's rounding and would give different results for
such coordinates.
