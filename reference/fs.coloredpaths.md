# Create fs.coloredpaths instance from 2 point matrices.

Create a renderable that draws the line segments from `from` to `to`,
with one color and (optionally) one width per segment. This is the line
equivalent of a coloredmesh, see the details.

## Usage

``` r
fs.coloredpaths(
  from,
  to,
  col = "#FF0000",
  width = 1,
  depth_test = TRUE,
  lit = FALSE,
  hemi = NULL,
  metadata = list(),
  render = TRUE,
  style = NULL
)
```

## Arguments

- from:

  numeric matrix of size (n, 3), the start points of the n segments.

- to:

  numeric matrix of size (n, 3), the end points of the n segments.

- col:

  vector of hex color strings, either a single one or one per segment.

- width:

  vector of positive numbers or a single one, the line width(s) in
  pixels.

- depth_test:

  logical, whether the lines should be hidden by geometry which is
  closer to the camera. Defaults to TRUE. Set to FALSE to draw the lines
  on top of everything, which is useful for annotations.

- lit:

  logical, whether to apply lighting to the lines. Defaults to FALSE,
  which means the color is used as-is (like hardware-rendered lines, and
  like
  [`segments3d`](https://dmurdoch.github.io/rgl/dev/reference/primitives.html)
  without a lit material).

- hemi:

  character string or NULL, the hemisphere this renderable belongs to.
  Defaults to NULL, which means that it is not hemisphere-specific and
  is rendered in the views of both hemispheres.

- metadata:

  named list, metadata for this renderable. See the details.

- render:

  logical, whether to render this instance. Meshes with `render=FALSE`
  are skipped by all rendering functions, see the parameter
  'skip_all_na' of
  [`vis.coloredmeshes`](https://dfsp-spirit.github.io/fsbrain/reference/vis.coloredmeshes.md).

- style:

  `NULL` or a rendering style for this renderable, see
  [`get.rglstyle`](https://dfsp-spirit.github.io/fsbrain/reference/get.rglstyle.md).

## Value

fs.coloredpaths instance. A named list with entries: "from" (the start
points), "to" (the end points), "col" (the colors), "width" (the line
widths), "depth_test", "lit", "hemi", "render", and "metadata".

## Details

Both renderer backends support this class. The rgl backend renders the
segments as hardware lines (rgl::segments3d), the headless scimesh
backend uses a scimesh line layer (see scimesh::line_layer), which
rasterizes the lines directly instead of creating tube geometry. Note
that the width of lines is measured in pixels and is a screen-space
property, so lines do not get thinner when the camera moves away (this
is what hardware line rendering does, and it also means that a line is
always visible, even when it is very thin).

The `metadata` field works exactly like the one of a coloredmesh: if it
contains the entries 'src_data' and 'makecmap_options', a colorbar can
be plotted for this renderable, see
[`coloredmesh.plot.colorbar.separate`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.plot.colorbar.separate.md).

## Examples

``` r
# A single white line segment from the origin to (10, 0, 0):
p = fs.coloredpaths(matrix(c(0, 0, 0), ncol = 3), matrix(c(10, 0, 0), ncol = 3), col = "#FFFFFF");
p;
#> Brain coloredpaths with 1 segment(s).
#>   widths: 1
#>   colors: #FFFFFF
#>   depth test: TRUE, lit: FALSE, render: TRUE

# Two segments in different colors, both of them with a width of 2 pixels:
from = matrix(c(0, 0, 0, 0, 0, 10), ncol = 3, byrow = TRUE);
to = matrix(c(10, 0, 0, 0, 10, 10), ncol = 3, byrow = TRUE);
p2 = fs.coloredpaths(from, to, col = c("#FF0000", "#00FF00"), width = 2);
```
