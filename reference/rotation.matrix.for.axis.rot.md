# Get rotation matrix for a 3D rotation around an axis.

Get rotation matrix for a 3D rotation around an axis.

## Usage

``` r
rotation.matrix.for.axis.rot(angle_rad, x, y, z, with_trans = TRUE)
```

## Arguments

- angle_rad:

  doule, the angle in radians

- x:

  rotation axis

- y:

  rotation axis

- z:

  rotation axis

- with_trans:

  logical, whether to extend the 3x3 rotation matrix to a 4x4 rotation
  and tranlsation matrix.

## Value

a 3x3 or 4x4 double matrix
