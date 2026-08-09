# Arrange a multi-frame ImageMagick image into a grid.

Arrange all subimages of the given ImageMagick image into a single 2D
image, that contains the subimages arranged in a grid-like structure.
Consecutive subimages will be appear the same row.

## Usage

``` r
magick.grid(
  magickimage,
  per_row = 5L,
  per_col = NULL,
  background_color = "#000000"
)
```

## Arguments

- magickimage:

  an ImageMagick image

- per_row:

  positive integer, the number of subimages per row in the output image.
  If `NULL`, automatically computed from the number of slices and the
  `per_col` parameter.

- per_col:

  positive integer, the number of subimages per column in the output
  image. If `NULL`, automatically computed from the number of slices and
  the `per_row` parameter.

- background_color:

  string, a valid ImageMagick color string such as "white" or "#000080".
  The color to use when extending images (e.g., when creating the
  border). Defaults to black.
