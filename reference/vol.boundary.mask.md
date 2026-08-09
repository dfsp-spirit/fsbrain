# Compute foreground pixels over the whole 3D imagestack.

Compute, over all images in a stack along an axis, the foreground and
background pixels as a binary mask. A pixel is a `foreground` pixel iff
its value is greater than the `threshold` parameter in at least one of
the slices. A pixel is a `background` pixel iff its value is below or
euqal to the `threshold` in all slices.

## Usage

``` r
vol.boundary.mask(volume, plane = 1L, threshold = 0L)
```

## Arguments

- volume:

  a 3D image volume

- plane:

  integer vector of length 2 or something that will be turned into one
  by
  [`vol.plane.axes`](https://dfsp-spirit.github.io/fsbrain/reference/vol.plane.axes.md).

- threshold:

  numerical, the threshold intensity used to separate background and
  foreground. All voxels with intensity values greater than this value
  will be considered `foreground` voxels.

## Value

integer 2D matrix with dimensions of a slice of the volume. Positions
set to 1 are `foreground` pixels and positions set to 0 are `background`
pixels (see `Details` section).
