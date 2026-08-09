# Copy the first *n* foreground voxel values.

Copy the first *n* foreground voxel values along the axis and direction
from the volume to the hull, thus adding foreground voxels to the hull.

## Usage

``` r
hull.retain.along.axis(
  volume,
  hull,
  dim_check = 2L,
  upwards = TRUE,
  thickness = 1L
)
```

## Arguments

- volume:

  numeric 3d array, the full source volume.

- hull:

  numeric 3d array, the input hull volume.

- dim_check:

  integer, the array dimension to use. Must be 1L, 2L or 3L.

- upwards:

  logical, whether to use upwards direction (increasing indices) in the
  array dimension

- thickness:

  integer, the width of the border in voxels, i.e., how many of the
  foreground voxels to keep

## Value

numeric 3d array, the updated hull volume.
