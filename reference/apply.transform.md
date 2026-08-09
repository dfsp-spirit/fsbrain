# Apply matmult transformation to input.

Apply affine transformation, like a *vox2ras_tkr* transformation, to
input. This is just matrix multiplication for different input objects.

## Usage

``` r
apply.transform(object, matrix_fun)
```

## Arguments

- object:

  numerical vector/matrix or Triangles3D instance, the coorindates or
  object to transform.

- matrix_fun:

  a 4x4 affine matrix or a function returning such a matrix. If `NULL`,
  the input is returned as-is. In many cases you way want to use a
  matrix computed from the header of a volume file, e.g., the `vox2ras`
  matrix of the respective volume. See the `mghheader.*` functions in
  the *freesurferformats* package to obtain these matrices.

## Value

the input after application of the affine matrix (matrix multiplication)
