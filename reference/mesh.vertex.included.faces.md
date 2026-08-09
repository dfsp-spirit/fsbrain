# Return all faces which are made up completely of the listed vertices.

Return all faces which are made up completely of the listed vertices.

## Usage

``` r
mesh.vertex.included.faces(surface_mesh, source_vertices)
```

## Arguments

- surface_mesh:

  surface mesh, as loaded by
  [`subject.surface`](https://dfsp-spirit.github.io/fsbrain/reference/subject.surface.md)
  or
  [`read.fs.surface`](https://rdrr.io/pkg/freesurferformats/man/read.fs.surface.html).

- source_vertices:

  integer vector, the vertex indices.

## Value

integer vector, the face indices

## See also

Other surface mesh functions:
[`face.edges()`](https://dfsp-spirit.github.io/fsbrain/reference/face.edges.md),
[`label.border()`](https://dfsp-spirit.github.io/fsbrain/reference/label.border.md),
[`mesh.vertex.neighbors()`](https://dfsp-spirit.github.io/fsbrain/reference/mesh.vertex.neighbors.md),
[`subject.surface()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.surface.md),
[`vis.path.along.verts()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.path.along.verts.md)
