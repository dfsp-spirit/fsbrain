# Compute all vertices within given geodesic distance on the mesh.

Compute all vertices within given geodesic distance on the mesh.

## Usage

``` r
geod.vert.neighborhood(
  mesh,
  vertex,
  max_distance = 5,
  include_max = TRUE,
  return_distances = TRUE
)
```

## Arguments

- mesh:

  an instance of
  [`rgl::tmesh3d`](https://dmurdoch.github.io/rgl/dev/reference/mesh3d.html)
  or `freesurferformats::fs.surface`.

- vertex:

  positive integer (or vector of the latter), the index of the source
  vertex in the mesh. If a vector, the neighborhoods for all vertices
  will be computed separately.

- max_distance:

  double, the neighborhood size. All mesh vertices in geodesic distance
  smaller than / up to this distance will be returned.

- include_max:

  logical, whether the max_distance value is inclusive.

- return_distances:

  logical, whether to compute the 'distances' entry in the returned
  list. Doing so is a little bit slower, so it can be turned off if not
  needed.

## Value

named list with the following entries: 'vertices': integer vector, the
indices of all vertices in the neigborhood. 'distances': double vector,
the distances to the respective vertices (unless 'return_distances' is
FALSE).

## Note

This function uses the pseudo-geodesic distance along the mesh edges.

## Examples

``` r
if (FALSE) { # \dontrun{
  sjd = fsaverage.path(TRUE);
  surface = subject.surface(sjd, 'fsaverage', surface = "white", hemi = "lh");
  res = geod.vert.neighborhood(surface, 12345L, max_distance = 10.0);
  res$vertices;
} # }
```
