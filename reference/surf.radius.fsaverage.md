# Get pre-computed radius for fsaverage white surface.

Get pre-computed radius for fsaverage white surface.

## Usage

``` r
surf.radius.fsaverage(maxr = TRUE)
```

## Arguments

- maxr:

  logical, whether to return the maximum of the x, y, and z radii.

## Value

If 'maxr' is TRUE, a numeric scalar representing the max of the x, y,
and z radii, otherwise, a numerical vector of length 3 with all radii.

## Note

The coordinates are for the white surface and in surface space, i.e.,
based on the raw values stored in the `fsaverae/surf/lh.white` and
`fsaverage/surf/rh.white` files, without applying any transformation.

## See also

[`surfs.props`](https://dfsp-spirit.github.io/fsbrain/reference/surfs.props.md),
which was used to compute the returned values.
