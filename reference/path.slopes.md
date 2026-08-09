# Compute slopes of paths relative to axes.

Compute slopes of paths relative to axes.

## Usage

``` r
path.slopes(coords_list, return_angles = FALSE)
```

## Arguments

- coords_list:

  list of `m` matrices, each `n` x 3 matrix must contain the 3D coords
  for one path.

- return_angles:

  logical, whether to return angles instead of slopes. Angles are
  returned in degrees, and will range from `-90` to `+90`.

## Value

`m` x 3 matrix, each row corresponds to a path and describes the 3
slopes of the path against the 3 planes/ x, y, and z axes (in that
order).
