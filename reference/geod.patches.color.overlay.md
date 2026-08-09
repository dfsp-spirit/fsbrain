# Generate color overlay from geodesic patches around several vertices.

Works across hemispheres (for a whole brain) if you pass a
[`hemilist`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.md)
of meshes as parameter 'mesh', see below.

## Usage

``` r
geod.patches.color.overlay(
  mesh,
  vertex,
  color = "#FF0000",
  bg_color = "#FEFEFE",
  ...
)
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

- color:

  single color string like `'#FF0000'` or vector of such strings. If a
  vector, the length should match the number of vertices in parameter
  'vertex'.

- bg_color:

  character string, the background color.

- ...:

  extra arguments passed to `geod.vert.neighborhood`.

## Value

vector of color strings (or a
[`hemilist`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.md)
of 2 such vectors if 'mesh' is a hemilist), an overlay suitable for
visualization using `vis.color.on.subject`.

## Examples

``` r
if (FALSE) { # \dontrun{
  sjd = fsaverage.path(TRUE);
  surfaces = subject.surface(sjd, 'fsaverage', surface = "white", hemi = "both");
  colors = geod.patches.color.overlay(surfaces, vertex = c(12345L, 45L),
    color = c("#FF0000", "#00FF00"), max_distance = 45.0);
  vis.color.on.subject(sjd, 'fsaverage', color_lh=colors$lh, color_rh=colors$rh);
} # }
```
