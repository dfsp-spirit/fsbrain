# Sample a 3D volume at the given voxel coordinates using trilinear interpolation.

Sample a 3D volume at the given voxel coordinates using trilinear
interpolation.

## Usage

``` r
vol.sample.trilinear(volume_data, coords)
```

## Arguments

- volume_data:

  numeric 3D array, the volume data.

- coords:

  numeric matrix with 3 columns and one row per query point, the
  **0-based** voxel coordinates (column, row, slice). Coordinates must
  be within the volume, or an error will be raised.

## Value

numeric vector, one interpolated value per query point.

## Note

Coordinates which are exactly on the last voxel result in the value of
that voxel: the interpolation weights of the out-of-range upper
neighbors become 0, and their index is clamped (which is safe because
their weight is 0).
