# Build a 4x4 rotation matrix (Rodrigues formula).

Build a 4x4 rotation matrix (Rodrigues formula).

## Usage

``` r
rotation.matrix(angle_rad, x, y, z)
```

## Arguments

- angle_rad:

  numeric scalar, the rotation angle in radians.

- x:

  numeric, x component of the rotation axis.

- y:

  numeric, y component of the rotation axis.

- z:

  numeric, z component of the rotation axis.

## Value

4x4 numeric rotation matrix in homogeneous coordinates. Matches the
convention of
[`rotate3d`](https://dmurdoch.github.io/rgl/dev/reference/matrices.html)
/
[`rotationMatrix`](https://dmurdoch.github.io/rgl/dev/reference/matrices.html).
