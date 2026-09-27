# Apply a 4x4 affine matrix to vertex coordinates.

Internal helper, applies the matrix to homogeneous column vectors, i.e.,
`v' = M %*% v`.

## Usage

``` r
apply.affine.to.coords(coords, affine_matrix)
```

## Arguments

- coords:

  Nx3 matrix of vertex coordinates (or a vector of length 3).

- affine_matrix:

  a 4x4 affine matrix.

## Value

Nx3 matrix of transformed coordinates.
