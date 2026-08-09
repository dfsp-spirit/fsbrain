# Generate per-vertex distance data from geodesic patches around several vertices for a single hemi.

Generate per-vertex distance data from geodesic patches around several
vertices for a single hemi.

## Usage

``` r
geod.patches.pervertexdata.singlehemi(mesh, vertex, ...)
```

## Arguments

- mesh:

  a single `fs.surface` instance.

- vertex:

  positive integer (or vector of the latter), the index of the source
  vertex in the mesh. If a vector, the neighborhoods for all vertices
  will be computed separately.

- ...:

  extra arguments passed to `geod.vert.neighborhood`.

## See also

geod.patches.pervertexdata
