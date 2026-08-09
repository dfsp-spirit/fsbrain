# Rotate and visualize coloredmeshes, applying a style.

Rotate and visualize coloredmeshes, applying a style.

## Usage

``` r
vis.rotated.coloredmeshes(
  renderables,
  rotation_angle,
  x,
  y,
  z,
  style = "default",
  draw_colorbar = FALSE
)
```

## Arguments

- rotation_angle:

  angle in radians. Passed to
  [`rotate3d`](https://dmurdoch.github.io/rgl/dev/reference/matrices.html).

- x:

  x value passed to
  [`rotate3d`](https://dmurdoch.github.io/rgl/dev/reference/matrices.html).

- y:

  y value passed to
  [`rotate3d`](https://dmurdoch.github.io/rgl/dev/reference/matrices.html).

- z:

  z value passed to
  [`rotate3d`](https://dmurdoch.github.io/rgl/dev/reference/matrices.html).

- style:

  a named list of style parameters or a string specifying an available
  style by name (e.g., 'shiny'). Defaults to 'default', the default
  style.

- draw_colorbar:

  logical. Whether to draw a colorbar.

- coloredmeshes:

  list of renderables. A coloredmesh is a named list as returned by the
  coloredmesh.from.\* functions. It has the entries 'mesh' of type
  tmesh3d, a 'col', which is a color specification for such a mesh.
