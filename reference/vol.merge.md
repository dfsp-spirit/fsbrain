# Merge background volume and overlay to new colors.

Merge background volume and overlay to new colors.

## Usage

``` r
vol.merge(
  volume,
  overlay_colors,
  bbox_threshold = 0L,
  forced_overlay_color = NULL
)
```

## Arguments

- volume:

  3D array, can be numeric (gray-scale intensity values) or color
  strings. If numeric, the intensity values must be in range `[0, 1]`.

- overlay_colors:

  3D array of color strings, values which are not part of the overlay
  (and should display background in the result) must have `NA` instead
  of a color string. Must have same dimensions as the `volume`.

- bbox_threshold:

  numerical, the threshold intensity used to separate background and
  foreground. All voxels with intensity values greater than this value
  in the background `volume` will be considered `foreground` voxels.
  Background-only slices at the borders of the volume will be discarded
  (in the merged, final image). Pass `NULL` to use the full image
  without applying any bounding box.

- forced_overlay_color:

  NULL or an rgb color string, like '#FF0000' for red. If NULL, the
  activation colors will be used as foreground colors. Otherwise, the
  given color will be for all foreground vertices.

## Value

3D array of color strings, the merged colors

## See also

Other volume utility:
[`vol.boundary.box()`](https://dfsp-spirit.github.io/fsbrain/reference/vol.boundary.box.md),
[`vol.imagestack()`](https://dfsp-spirit.github.io/fsbrain/reference/vol.imagestack.md),
[`vol.overlay.colors.from.activation()`](https://dfsp-spirit.github.io/fsbrain/reference/vol.overlay.colors.from.activation.md),
[`vol.planes()`](https://dfsp-spirit.github.io/fsbrain/reference/vol.planes.md),
[`vol.slice()`](https://dfsp-spirit.github.io/fsbrain/reference/vol.slice.md)
