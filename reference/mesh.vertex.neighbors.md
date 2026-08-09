# Compute neighborhood of a vertex

Given a set of query vertex indices and a mesh *m*, compute all vertices
which are adjacent to the query vertices in the mesh. A vertex *u* is
*adjacent* to another vertex *v* iff there exists an edge *e = (u, v)*
in *m*. While you could call this function repeatedly with the old
output as its new input to extend the neighborhood, you should maybe use
a proper graph library for this.

## Usage

``` r
mesh.vertex.neighbors(
  surface,
  source_vertices,
  k = 1L,
  restrict_to_vertices = NULL
)
```

## Arguments

- surface:

  a surface as returned by functions like
  [`subject.surface`](https://dfsp-spirit.github.io/fsbrain/reference/subject.surface.md)
  or
  [`read.fs.surface`](https://rdrr.io/pkg/freesurferformats/man/read.fs.surface.html).

- source_vertices:

  Vector of source vertex indices.

- k:

  positive integer, how often to repeat the procedure and grow the
  neighborhood, using the output `vertices` as the `source_vertices` for
  the next iteration. Warning: settings this to high values will be very
  slow for large meshes.

- restrict_to_vertices:

  integer vector of vertex indices. If given, the neighborhood growth
  will be limited to the given vertex indices. Defaults to NULL, which
  means the neighborhood is not restricted.

## Value

the neighborhood as a list with two entries: "faces": integer vector,
the face indices of all faces the source_vertices are a part of.
"vertices": integer vector, the unique vertex indices of all vertices of
the faces in the 'faces' property. These vertex indices include the
indices of the source_vertices themselves.

## See also

Other surface mesh functions:
[`face.edges()`](https://dfsp-spirit.github.io/fsbrain/reference/face.edges.md),
[`label.border()`](https://dfsp-spirit.github.io/fsbrain/reference/label.border.md),
[`mesh.vertex.included.faces()`](https://dfsp-spirit.github.io/fsbrain/reference/mesh.vertex.included.faces.md),
[`subject.surface()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.surface.md),
[`vis.path.along.verts()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.path.along.verts.md)
