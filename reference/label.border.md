# Compute border of a label.

Compute the border of a label (i.e., a subset of the vertices of a
mesh). The border thickness can be specified. Useful to draw the outline
of a region, e.g., a significant cluster on the surface or a part of a
ROI from a brain parcellation.

## Usage

``` r
label.border(
  surface_mesh,
  label,
  inner_only = TRUE,
  expand_inwards = 0L,
  derive = FALSE
)
```

## Arguments

- surface_mesh:

  surface mesh, as loaded by
  [`subject.surface`](https://dfsp-spirit.github.io/fsbrain/reference/subject.surface.md)
  or
  [`read.fs.surface`](https://rdrr.io/pkg/freesurferformats/man/read.fs.surface.html).

- label:

  instance of class `fs.label` or an integer vector, the vertex indices.
  This function only makes sense if they form a patch on the surface,
  but that is not checked.

- inner_only:

  logical, whether only faces consisting only of label_vertices should
  be considered to be label faces. If FALSE, faces containing at least
  one label vertex will be used. Defaults to TRUE. Leave this alone if
  in doubt, especially if you want to draw several label borders which
  are directly adjacent on the surface.

- expand_inwards:

  integer, border thickness extension. If given, once the border has
  been computed, it is extended by the given graph distance. It is
  guaranteed that the border only extends inwards, i.e., it will never
  extend to vertices which are not part of the label.

- derive:

  logical, whether the returned result should also include the border
  edges and faces in addition to the border vertices. Takes longer if
  requested, defaults to FALSE.

## Value

the border as a list with the following entries: `vertices`: integer
vector, the vertex indices of the border. Iff the parameter `derive` is
TRUE, the following two additional fields are included: `edges`: integer
matrix of size (n, 2) for n edges. Each row defines an edge by its start
and target vertex. `faces`: integer vector, the face indices of the
border.

## See also

Other surface mesh functions:
[`face.edges()`](https://dfsp-spirit.github.io/fsbrain/reference/face.edges.md),
[`mesh.vertex.included.faces()`](https://dfsp-spirit.github.io/fsbrain/reference/mesh.vertex.included.faces.md),
[`mesh.vertex.neighbors()`](https://dfsp-spirit.github.io/fsbrain/reference/mesh.vertex.neighbors.md),
[`subject.surface()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.surface.md),
[`vis.path.along.verts()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.path.along.verts.md)
