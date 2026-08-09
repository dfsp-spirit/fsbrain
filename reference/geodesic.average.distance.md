# Compute the average (pseudo-) geodesic distance on the mesh from each vertex to all other vertices.

Compute the average (pseudo-) geodesic distance on the mesh from each
vertex to all other vertices.

## Usage

``` r
geodesic.average.distance(surfaces)
```

## Arguments

- surfaces:

  fs.surface instance or a
  [`hemilist`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.md)
  of the latter.

## Note

This may take a while. It requires the 'Rvcg' package.
