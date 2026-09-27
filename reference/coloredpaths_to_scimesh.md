# Convert an fs.coloredpaths instance to scimesh line layers

Line segments have no mesh representation, so they cannot be passed to
the scimesh renderer as meshes. They are converted to scimesh line
layers instead (see
[`scimesh::line_layer`](https://rdrr.io/pkg/scimesh/man/line_layer.html)),
which the scimesh rasterizer draws directly, without creating any
geometry. This is the cheap way to draw many thin lines, like the edges
of a connectome.

## Usage

``` r
coloredpaths_to_scimesh(cpaths, style = "default")
```

## Arguments

- cpaths:

  an fs.coloredpaths instance.

- style:

  a rendering style, see
  [`get.rglstyle`](https://dfsp-spirit.github.io/fsbrain/reference/get.rglstyle.md).

## Value

a list of scimesh line layers (class 'scimesh_lines'). One layer per
distinct line width, because the width is a property of the layer.
