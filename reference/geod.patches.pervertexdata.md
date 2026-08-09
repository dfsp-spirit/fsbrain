# Generate per-vertex distance data from geodesic patches around several vertices.

Works across hemispheres (for a whole brain) if you pass a
[`hemilist`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.md)
of meshes as parameter 'mesh', see below.

## Usage

``` r
geod.patches.pervertexdata(mesh, vertex, ...)
```

## Arguments

- mesh:

  a single `fs.surface` instance, or a
  [`hemilist`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.md)
  of two such meshes. If a hemilist, the vertex indices can go from 1 to
  the sum of vertices in both meshes, and the proper hemisphere will be
  used automatically.

- vertex:

  positive integer (or vector of the latter), the index of the source
  vertex in the mesh. If a vector, the neighborhoods for all vertices
  will be computed separately.

- ...:

  extra arguments passed to `geod.vert.neighborhood`.

## Value

vector of doubles (or a
[`hemilist`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.md)
of 2 such vectors if 'mesh' is a hemilist), the per-vertex distance
data. Data for vertices outside neighborhoods will be NA.

## Examples

``` r
if (FALSE) { # \dontrun{
  sjd = fsaverage.path(TRUE);
  surfaces = subject.surface(sjd, 'fsaverage',
    surface = "white", hemi = "both");
  res = geod.patches.pervertexdata(surfaces,
    vertex = c(12345L, 45L),
    max_distance = 25.0);
  # res$lh and res$rh now hold the per-vertex data.
} # }
```
