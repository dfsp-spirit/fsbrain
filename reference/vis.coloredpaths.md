# Draw the segments of an fs.coloredpaths instance with rgl.

Uses rgl::segments3d, i.e., hardware lines, so that a line is always one
pixel wide (times the requested width) no matter how far away it is from
the camera. Segments which share a line width are drawn in a single
call, because the line width is a material property (like the color,
which can be set per segment).

## Usage

``` r
vis.coloredpaths(cpaths, style = "default")
```

## Arguments

- cpaths:

  an fs.coloredpaths instance.

- style:

  a rendering style, see
  [`get.rglstyle`](https://dfsp-spirit.github.io/fsbrain/reference/get.rglstyle.md).

## Value

invisible NULL.
