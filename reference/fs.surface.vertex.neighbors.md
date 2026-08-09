# Compute vertex neighborhoods or the full adjacency list for a mesh using the Rvcg or igraph library.

This is a faster replacement for `mesh.vertex.neighbors` that requires
the optional dependency package 'igraph' or 'Rvcg'.

## Usage

``` r
fs.surface.vertex.neighbors(
  surface,
  nodes = NULL,
  order = 1L,
  simplify = TRUE,
  include_self = FALSE
)
```

## Arguments

- surface:

  an fs.surface instance as returned by `subject.surface`, an existing
  igraph (which will be returned as-is) or a string which is interpreted
  as a path to a surface file.

- nodes:

  the source vertex. Passed on to
  [`igraph::neighborhood`](https://r.igraph.org/reference/ego.html). Can
  be a vector, in which case the neighborhoods for all these vertices
  are computed separately. If NULL, all graph vertices are used.

- order:

  integer, the max graph distance of vertices to consider neighbors
  (number of neighborhood rings). Passed on to
  [`igraph::neighborhood`](https://r.igraph.org/reference/ego.html)

- simplify:

  logical, whether to return only an integer vector if the 'nodes'
  parameter has length 1 (instead of a list where the first element is
  such a vector).

- include_self:

  logical, whether to include vertices in their own neighborhood

## Value

named list of integer vectors (see
[`igraph::neighborhood`](https://r.igraph.org/reference/ego.html)),
unless 'simplify' is TRUE, see there for details.

## Note

If you intend to call several functions on the igraph, it is faster to
construct it with `fs.surface.to.igraph` and keep it.

## See also

The `fs.surface.as.adjacencylist` function computes the 1-ring
neighborhood for the whole graph.
