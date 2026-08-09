# Draw a 3D line from vertex to vertex

To get a nice path along the surface, pass the vertex indices along a
geodesic path. Note: You can first open an interactive brain view
(`views='si'`) with a vis\* function like
[`vis.subject.morph.native`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.morph.native.md),
then run this function to draw into the active plot.

## Usage

``` r
vis.path.along.verts(
  surface_vertices,
  path_vertex_indices = NULL,
  do_vis = TRUE,
  color = "#FF0000",
  no_material = FALSE
)
```

## Arguments

- surface_vertices:

  float matrix of size (n, 3), the surface vertex coordinates, as
  returned as part of
  [`subject.surface`](https://dfsp-spirit.github.io/fsbrain/reference/subject.surface.md)
  or
  [`read.fs.surface`](https://rdrr.io/pkg/freesurferformats/man/read.fs.surface.html),
  in the member "vertices". Can also be a
  `freesurferformats::fs.surface` or
  [`rgl::tmesh3d`](https://dmurdoch.github.io/rgl/dev/reference/mesh3d.html)
  instance, in which case the coordinates are extracted automatically.

- path_vertex_indices:

  vector of vertex indices, the path. You will need to have it computed
  already. (This function does **not** compute geodesic paths, see
  [`geodesic.path`](https://dfsp-spirit.github.io/fsbrain/reference/geodesic.path.md)
  for that. You can use it to visualize such a path though.) If omitted,
  the vertex coordinates will be traversed in their given order to
  create the path.

- do_vis:

  logical, whether to actually draw the path.

- color:

  a color string, like '#FF0000' to color the path.

- no_material:

  logical, whether to use set the custom rendering material properties
  for path visualization using
  [`rgl::material3d`](https://dmurdoch.github.io/rgl/dev/reference/material.html)
  before plotting. If you set this to FALSE, no material will be set and
  you should set it yourself before calling this function, otherwise the
  looks of the path are undefined (dependent on the default material on
  your system, or the last material call). Setting this to TRUE also
  means that the 'color' argument is ignored of course, as the color is
  part of the material.

## Value

n x 3 matrix, the coordinates of the path, with appropriate ones
duplicated for rgl pair-wise segments3d rendering.

## See also

[`vis.paths`](https://dfsp-spirit.github.io/fsbrain/reference/vis.paths.md)
if you need to draw many paths,
[`geodesic.path`](https://dfsp-spirit.github.io/fsbrain/reference/geodesic.path.md)
to compute a geodesic path.

Other surface mesh functions:
[`face.edges()`](https://dfsp-spirit.github.io/fsbrain/reference/face.edges.md),
[`label.border()`](https://dfsp-spirit.github.io/fsbrain/reference/label.border.md),
[`mesh.vertex.included.faces()`](https://dfsp-spirit.github.io/fsbrain/reference/mesh.vertex.included.faces.md),
[`mesh.vertex.neighbors()`](https://dfsp-spirit.github.io/fsbrain/reference/mesh.vertex.neighbors.md),
[`subject.surface()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.surface.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  sjd = fsaverage.path(TRUE);
  surface = subject.surface(sjd, 'fsaverage3',
    surface = "white", hemi = "lh");
  p = geodesic.path(surface, 5, c(10, 20));
  vis.subject.morph.native(sjd, 'fsaverage3', views='si');
  vis.path.along.verts(surface$vertices, p[[1]]);
} # }
```
