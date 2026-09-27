# Get the default visualization style parameters as a named list.

Run
[`material3d`](https://dmurdoch.github.io/rgl/dev/reference/material.html)
without arguments to see valid style keywords to create new styles.

## Usage

``` r
get.rglstyle(style)
```

## Arguments

- style:

  string. A style name. Available styles are one of: "default", "shiny",
  "semitransparent", "glass", "edges".

## Value

a style, resolved to a parameter list compatible with
[`material3d`](https://dmurdoch.github.io/rgl/dev/reference/material.html).

## Note

In addition to the style names listed above, the magic word 'from_mesh'
can be passed as parameter 'style' to the rendering functions: it makes
them use the style stored in the 'style' field of each individual mesh
(see
[`coloredmesh.from.color`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.color.md)).
This allows you to give individual meshes of a scene their own look,
e.g., to render a cortex mesh semi-transparently behind colored data
meshes, see
[`vis.subcortical.region.values`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subcortical.region.values.md).
Note that the scimesh renderer backend currently only supports the alpha
channel of a style, not other material properties.

## See also

[`shade3d`](https://dmurdoch.github.io/rgl/dev/reference/shade3d.html)
can use the returned style
