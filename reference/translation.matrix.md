# Create a 4x4 translation matrix.

Create the affine matrix which translates coordinates by the given
offsets, for use with
[`apply.transform`](https://dfsp-spirit.github.io/fsbrain/reference/apply.transform.md).

## Usage

``` r
translation.matrix(x = 0, y = 0, z = 0)
```

## Arguments

- x:

  numerical scalar, the translation along the first axis.

- y:

  numerical scalar, the translation along the second axis.

- z:

  numerical scalar, the translation along the third axis.

## Value

numeric 4x4 matrix, the translation matrix.
