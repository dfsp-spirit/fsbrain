# Simple internal wrapper around `Rvcg::vcgDijkstra` with function check.

Simple internal wrapper around
[`Rvcg::vcgDijkstra`](https://rdrr.io/pkg/Rvcg/man/vcgDijkstra.html)
with function check.

## Usage

``` r
geodesic.dists.to.vertex(mesh, v)
```

## Arguments

- mesh:

  a tmesh3d instance.

- v:

  positive integer, a vertex index in the mesh.

## Value

double vector with length equal to num vertices in the mesh, the
geodesic distances from all other vertices to the query vertex `v`.
