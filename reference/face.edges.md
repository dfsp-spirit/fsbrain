# Enumerate all edges of the given faces or mesh.

Compute edges of a tri-mesh. Can compute all edges, or only a subset,
given by the face indices in the mesh.

## Usage

``` r
face.edges(surface_mesh, face_indices = "all")
```

## Arguments

- surface_mesh:

  surface mesh, as loaded by
  [`subject.surface`](https://dfsp-spirit.github.io/fsbrain/reference/subject.surface.md)
  or
  [`read.fs.surface`](https://rdrr.io/pkg/freesurferformats/man/read.fs.surface.html).

- face_indices:

  integer vector, the face indices. Can also be the character string
  'all' to use all faces.

## Value

integer matrix of size (n, 2) where n is the number of edges. The
indices (source and target vertex) in each row are **not** sorted, and
the edges are **not** unique. I.e., each undirected edge `u, v` (with
the exception of edges on the mesh border) will occur twice in the
result: once as `u, v` and once as `v, u`.

## See also

Other surface mesh functions:
[`label.border()`](https://dfsp-spirit.github.io/fsbrain/reference/label.border.md),
[`mesh.vertex.included.faces()`](https://dfsp-spirit.github.io/fsbrain/reference/mesh.vertex.included.faces.md),
[`mesh.vertex.neighbors()`](https://dfsp-spirit.github.io/fsbrain/reference/mesh.vertex.neighbors.md),
[`subject.surface()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.surface.md),
[`vis.path.along.verts()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.path.along.verts.md)
