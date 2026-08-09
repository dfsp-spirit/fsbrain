# Draw contour segments onto a magick image.

Draws a set of line segments (as returned by
[`mesh.slice.intersection`](https://dfsp-spirit.github.io/fsbrain/reference/mesh.slice.intersection.md))
onto a magick image. Individual mesh triangles produce small segments,
which collectively trace the contour boundary when drawn together.

## Usage

``` r
draw.segments.on.image(
  img,
  segments,
  slice_axis,
  row_axis,
  col_axis,
  color = "#FF0000",
  lwd = 1
)
```

## Arguments

- img:

  a magick image instance (a single 2D slice image).

- segments:

  list of 2×2 matrices, each a line segment in 0-based CRS coordinates
  on the two orthogonal axes.

- slice_axis:

  integer, the axis (1,2,3) perpendicular to the slice plane.

- row_axis:

  integer, which CRS axis (1, 2, or 3) maps to the image row dimension.

- col_axis:

  integer, which CRS axis (1, 2, or 3) maps to the image column
  dimension.

- color:

  character string, the color for the contour lines.

- lwd:

  numeric, line width passed to
  [`segments`](https://rdrr.io/r/graphics/segments.html). Defaults to 1.

## Value

the modified magick image (invisibly).
