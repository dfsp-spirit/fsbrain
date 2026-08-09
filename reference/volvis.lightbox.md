# Draw a lightbox view from volume slices.

A lightbox is a single image that holds a set of subimages, arranged in
a grid. The images can have a small border or spacing between them.
Consecutive subimages will be appear the same row of the grid.

If overlay_colors are given, the volume will be used as the background,
and it will only be visible where overlay_colors has transparency.

## Usage

``` r
volvis.lightbox(
  volume,
  slices = -5,
  axis = 1L,
  per_row = 5L,
  per_col = NULL,
  border_geometry = "5x5",
  background_color = "#000000",
  arrange_single_image = FALSE
)
```

## Arguments

- volume:

  3D array, can be numeric (gray-scale intensity values) or color
  strings. If numeric, the intensity values must be in range `[0, 1]`.

- slices:

  slice index definition. If a vector of integers, interpreted as slice
  indices. If a single negative interger `-n`, interpreted as every
  `nth` slice, starting at slice 1. The character string 'all' or the
  value `NULL` will be interpreted as *all slices*.

- axis:

  positive integer in range 1L..3L, the axis to use.

- per_row:

  positive integer, the number of subimages per row in the output image.
  If `NULL`, automatically computed from the number of slices and the
  `per_col` parameter.

- per_col:

  positive integer, the number of subimages per column in the output
  image. If `NULL`, automatically computed from the number of slices and
  the `per_row` parameter.

- border_geometry:

  string, a geometry string passed to
  [`magick::image_border`](https://docs.ropensci.org/magick/reference/composite.html)
  to define the borders to add to each image tile. The default value
  adds 5 pixels, both horizontally and vertically.

- background_color:

  string, a valid ImageMagick color string such as "white" or "#000080".
  The color to use when extending images (e.g., when creating the
  border). Defaults to black.

- arrange_single_image:

  logical, whether to apply the given arrangement (from parameters
  `per_row` and `per_column`) even if a single slice (a 2D image) is
  passed as `volume`. Defaults to FALSE, which prevents that background
  tiles are added to fill the row up to `per_row` images. This also
  prevents the border from getting added to a single image, so all you
  see is the raw image. Set to `TRUE` if you want to arrange even a
  single image in a row with a border.

## Value

a magick image instance

## Note

You should, in most cases, not call this function directly. Use
[`volvis.lb`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.lb.md)
instead, which has a more intuitive interface.

## See also

[`volvis.lb`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.lb.md)

Other volume visualization:
[`vis.volume.on.surface()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.volume.on.surface.md),
[`volvis.lb()`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.lb.md),
[`volvis.lb.with.surface()`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.lb.with.surface.md),
[`volvis.slices.with.surface()`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.slices.with.surface.md)
