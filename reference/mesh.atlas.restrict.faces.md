# Remove all faces that use one of the given vertices and renumber the remaining ones.

Filter the faces of a mesh and renumber the vertex indices to refer to
the reduced vertex list.

## Usage

``` r
mesh.atlas.restrict.faces(faces, keep_mask)
```

## Arguments

- faces:

  integer matrix, one face per column, holding 1-based indices into the
  vertex list.

- keep_mask:

  logical vector, one entry per vertex of the mesh, TRUE for the
  vertices which are kept.

## Value

integer matrix like `faces`, with all faces removed that used a vertex
which is not kept, and with the remaining vertex indices renumbered to
refer to `which(keep_mask)`.
