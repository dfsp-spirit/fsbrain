# Apply an affine transformation matrix to an object.

Internal workhorse of
[`apply.transform`](https://dfsp-spirit.github.io/fsbrain/reference/apply.transform.md),
assumes that the matrix has already been resolved from the 'matrix_fun'
parameter.

## Usage

``` r
apply.transform.matrix(object, affine_matrix)
```

## Arguments

- object:

  the object to transform.

- affine_matrix:

  a 4x4 affine matrix.

## Value

the transformed object.
