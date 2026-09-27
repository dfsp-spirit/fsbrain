# Subdivide all triangles of a mesh (one subdivision step).

Splits every triangle into 4 by adding the edge midpoints, and projects
the new vertices onto the sphere, i.e., the new vertices are scaled to
unit length. This is the standard icosphere refinement, and it requires
the input to be a sphere centered at the origin.

## Usage

``` r
subdivide.triangles(vertices, faces)
```

## Arguments

- vertices:

  n x 3 numeric matrix, the vertex coordinates.

- faces:

  m x 3 integer matrix, the triangle vertex indices (1-based).

## Value

named list with entries 'vertices' and 'faces', the refined mesh.
