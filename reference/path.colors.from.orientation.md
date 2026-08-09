# Compute path color from its orientation.

Compute path color from its orientation.

## Usage

``` r
path.colors.from.orientation(coords_list, use_three_colors_only = FALSE)
```

## Arguments

- coords_list:

  list of `m` matrices, each `n` x 3 matrix must contain the 3D coords
  for one path.

- use_three_colors_only:

  logical, whether to use only three different colors, based on closest
  axis.

## Value

`m` x 3 matrix, each row corresponds to a path and contains its color
value (RGB, range 0..255).
