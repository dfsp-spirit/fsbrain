# Sample a 3D volume at world coordinates.

This is the core of the volume-to-surface projection: the world
coordinates (e.g., surface vertex positions) are transformed into voxel
coordinates using the inverse of the affine matrix, and the volume is
then sampled at those coordinates.

## Usage

``` r
vol.sample.at.coords(
  volume_data,
  affine,
  coords,
  interpolation = "trilinear",
  clamp = FALSE,
  check_outside = TRUE
)
```

## Arguments

- volume_data:

  numeric 3D array, the volume data.

- affine:

  numeric 4x4 matrix, maps 0-based voxel indices (column, row, slice) to
  world coordinates.

- coords:

  numeric matrix with 3 columns and one row per query point, the world
  coordinates at which to sample the volume.

- interpolation:

  character string, one of 'trilinear' (or its alias 'linear') or
  'nearest'.

- clamp:

  logical, whether to clamp query points which are outside the volume to
  the closest voxel at the volume border (edge replication). If FALSE,
  such points get the value NA.

- check_outside:

  logical, whether to determine which of the query points are outside
  the volume. Setting this to FALSE saves the (small) cost of the
  comparison, but the caller cannot report or mask such points.

## Value

named list with entries 'values' (numeric vector) and 'outside' (logical
vector or NULL if `check_outside` is FALSE).
