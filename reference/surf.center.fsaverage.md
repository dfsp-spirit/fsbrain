# Get pre-computed center for fsaverage white surface.

Get pre-computed center for fsaverage white surface.

## Usage

``` r
surf.center.fsaverage()
```

## Value

A numerical vector of length 3 with the x, y, and z coordinates of the
center. The center was computed as the point halfway between the min and
max mesh coordinates, on each axis separately.

## Note

The coordinates are for the white surface and in surface space, i.e.,
based on the raw values stored in the `fsaverae/surf/lh.white` and
`fsaverage/surf/rh.white` files, without applying any transformation.

## See also

[`surfs.props`](https://dfsp-spirit.github.io/fsbrain/reference/surfs.props.md),
which was used to compute the returned values.
