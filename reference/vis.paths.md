# Visualize many paths.

Visualize many paths.

## Usage

``` r
vis.paths(coords_list, path_color = "#FF0000")
```

## Arguments

- coords_list:

  list of `m` matrices, each `n` x 3 matrix must contain the 3D coords
  for one path.

- path_color:

  a color value, the color in which to plot the paths.

## Note

This function is a lot faster than calling `vis.path.along.verts` many
times and having it draw each time.
