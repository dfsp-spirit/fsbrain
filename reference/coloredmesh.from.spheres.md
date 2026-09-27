# Create a coloredmesh that draws a set of spheres (e.g., connectome nodes).

Builds one mesh containing all spheres, with a per-vertex color taken
from the color of the sphere the vertex belongs to. The result is a
regular
[`fs.coloredmesh`](https://dfsp-spirit.github.io/fsbrain/reference/fs.coloredmesh.md)
instance, so it can be rendered, transformed and exported like any other
mesh. Note that the vertices of a sphere are not shared with any other
sphere, so each sphere can have its own radius and color.

## Usage

``` r
coloredmesh.from.spheres(
  centers,
  radii = 1,
  col = "#FF0000",
  subdivisions = 2L,
  metadata = list(),
  hemi = NULL,
  style = NULL
)
```

## Arguments

- centers:

  n x 3 numeric matrix, the sphere centers.

- radii:

  numeric vector of length n or a single number, the sphere radii.

- col:

  vector of hex color strings, a single one or one per sphere.

- subdivisions:

  non-negative integer, the number of subdivisions of the unit
  icosphere. Defaults to 2L.

- metadata:

  named list, metadata for the resulting coloredmesh. See
  [`fs.coloredpaths`](https://dfsp-spirit.github.io/fsbrain/reference/fs.coloredpaths.md)
  for the metadata convention which enables colorbars.

- hemi:

  character string or NULL, the hemisphere this renderable belongs to.
  Defaults to NULL (not hemisphere-specific).

- style:

  `NULL` or a rendering style for this mesh, see
  [`get.rglstyle`](https://dfsp-spirit.github.io/fsbrain/reference/get.rglstyle.md).

## Value

fs.coloredmesh instance.

## Examples

``` r
  centers = rbind(c(0, 0, 0), c(10, 0, 0));
  cm = fsbrain:::coloredmesh.from.spheres(centers, c(1, 2), c("#FF0000", "#00FF00"));
  class(cm);
#> [1] "fs.coloredmesh" "list"          
```
