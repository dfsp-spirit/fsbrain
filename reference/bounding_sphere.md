# Compute the bounding sphere of a set of 3D vertices.

Computes the sphere that encloses the axis-aligned bounding box (AABB)
of the given vertices, using the same convention as rgl's `Sphere(AABB)`
auto-framing (see rgl src/geom.cpp) and scimesh's
`camera_auto(rgl_compat=TRUE)`: the center is the AABB center and the
radius is half the length of the AABB diagonal.

## Usage

``` r
bounding_sphere(vertices)
```

## Arguments

- vertices:

  an Nx3 numeric matrix of vertex coordinates, or a `mesh3d`/`tmesh3d`
  object, or an `fs.surface`, or a list of any of these (all vertices
  are pooled).

## Value

a list with entries `center` (numeric vector of length 3) and `radius`
(numeric scalar). For an empty/zero-extent input, `radius` is 0 and the
center is the single vertex (or `NA` if no vertices).
