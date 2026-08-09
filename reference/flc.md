# Given a list of path coordinates, create matrix containing only the first and last point of each path.

Given a list of path coordinates, create matrix containing only the
first and last point of each path.

## Usage

``` r
flc(coords_list)
```

## Arguments

- coords_list:

  list of `m` matrices, each `n` x 3 matrix must contain the 3D coords
  for one path.

## Value

m x 6 numeric matrix, containing the first and last point of a path per
row (two 3D xyz-coords, so 6 values per row).
