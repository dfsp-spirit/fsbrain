# Compute the vertices and faces of a unit icosphere (subdivision surface of the icosahedron).

The returned sphere has radius 1 and is centered at the origin. The
number of vertices is `10 * 4^subdivisions + 2` and the number of faces
is `20 * 4^subdivisions` (42 and 80 for 1 subdivision, 162 and 320 for
2).

## Usage

``` r
unit.icosphere(subdivisions = 2L)
```

## Arguments

- subdivisions:

  non-negative integer, the number of subdivision steps. 0 gives the
  plain icosahedron. Defaults to 2L, which is smooth enough for
  plot-sized spheres.

## Value

named list with entries 'vertices' (n x 3 numeric matrix) and 'faces' (m
x 3 integer matrix), i.e., an `freesurferformats::fs.surface` instance.
