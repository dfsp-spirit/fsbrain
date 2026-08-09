# Return coordinates for vertices, supporting entire brain via hemilist.

Return coordinates for vertices, supporting entire brain via hemilist.

## Usage

``` r
vertex.coords(surface, vertices)
```

## Arguments

- surface:

  an fs.surface instance, see
  [`subject.surface`](https://dfsp-spirit.github.io/fsbrain/reference/subject.surface.md)
  function. Can also be a hemilist of surfaces, in which case the
  vertices must be indices over both meshes (in range
  `1..(nv(lh)+nv(rh))`). If a hemilist, both entries must be surfaces
  (non-NULL).

- vertices:

  vector of positive integers, the vertex indices. Values which are
  outside of the valid indices for the surface will be silently ignored,
  making it easier to work with the two hemispheres.

## Value

double nx3 matrix of vertex coordinates.

## See also

Other 3d utility functions:
[`highlight.points.spheres()`](https://dfsp-spirit.github.io/fsbrain/reference/highlight.points.spheres.md),
[`highlight.vertices.spheres()`](https://dfsp-spirit.github.io/fsbrain/reference/highlight.vertices.spheres.md)
