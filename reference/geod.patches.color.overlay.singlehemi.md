# Generate color overlay from geodesic patches around several vertices for a single hemi.

Generate color overlay from geodesic patches around several vertices for
a single hemi.

## Usage

``` r
geod.patches.color.overlay.singlehemi(
  mesh,
  vertex,
  color = "#FF0000",
  bg_color = "#FEFEFE",
  ...
)
```

## Arguments

- mesh:

  a single `fs.surface` instance.

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
