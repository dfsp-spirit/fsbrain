# Compute 3D bounding box of a volume.

Compute the axis-aligned foreground bounding box of a 3D volume, i.e.
the inner foreground area that must be retained if you want to remove
all background from the corners of the volume. The foreground is
determined by thresholding, such that all values greater than 0 are
considered foreground. See
[`vol.boundary.mask`](https://dfsp-spirit.github.io/fsbrain/reference/vol.boundary.mask.md)
for details.

## Usage

``` r
vol.boundary.box(volume, threshold = 0L, apply = FALSE)
```

## Arguments

- volume:

  a 3D image volume

- threshold:

  numerical, the threshold intensity used to separate background and
  foreground. All voxels with intensity values greater than this value
  will be considered `foreground` voxels.

- apply:

  logical, whether to directly apply the bounding box and return the
  resulting volume instead.

## Value

named list with 2 entries: `from` is an integer vector of length 3,
defining the minimal (x,y,z) foreground indices. `to` is an integer
vector of length 3, defining the maximal (x,y,z) foreground indices.

## See also

Other volume utility:
[`vol.imagestack()`](https://dfsp-spirit.github.io/fsbrain/reference/vol.imagestack.md),
[`vol.merge()`](https://dfsp-spirit.github.io/fsbrain/reference/vol.merge.md),
[`vol.overlay.colors.from.activation()`](https://dfsp-spirit.github.io/fsbrain/reference/vol.overlay.colors.from.activation.md),
[`vol.planes()`](https://dfsp-spirit.github.io/fsbrain/reference/vol.planes.md),
[`vol.slice()`](https://dfsp-spirit.github.io/fsbrain/reference/vol.slice.md)
