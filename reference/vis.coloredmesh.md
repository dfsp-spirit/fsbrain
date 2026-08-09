# Draw a coloredmesh using a style.

Draw a coloredmesh using a style.

## Usage

``` r
vis.coloredmesh(cmesh, style = "default")
```

## Arguments

- cmesh:

  a coloredmesh. A coloredmesh is a named list as returned by the
  coloredmesh.from.\* functions. It has the entries 'mesh' of type
  tmesh3d, a 'col', which is a color specification for such a mesh.

- style:

  a named list of style parameters or a string specifying an available
  style by name (e.g., 'shiny'). Defaults to 'default', the default
  style. Pass the magic word 'from_mesh' to try to retrieve a style (as
  a name or a style list) from the field `style` of the mesh, or default
  to "default" if the mesh has no such field.

## See also

[`vis.renderable`](https://dfsp-spirit.github.io/fsbrain/reference/vis.renderable.md)
