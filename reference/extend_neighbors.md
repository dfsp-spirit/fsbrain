# Recursive computation of neighborhoods, see surf.sphere.dist

Compute neighborhood of the current vertex (=target vertex). The
computation follows the mesh edges while there are still vertices which
fullfil the dotproduct distance threshold.

## Usage

``` r
extend_neighbors(
  spherical_surface,
  targetvidx,
  currentvidx,
  min_dotp_thresh,
  ref_visited,
  ref_neighbors,
  ref_neighbor_dpdists
)
```

## Arguments

- spherical_surface:

  an fs.surface instance representing the spherical version (`lh.sphere`
  or `rh.sphere` of the subject).

- targetvidx:

  positive integer, initial target vertex. The vertex for which to
  compute the neighborhood.

- currentvidx:

  positive integer, initial current vertex. Pass identical value as in
  targetvidx, this is changed later in the recursion.

- min_dotp_thresh:

  double, the minimal dotproduct distance threshold to use. Only
  vertices along the structural mesh neighborhood with values greater
  this will be included in the neighborhood. Yes, greater.

- ref_visited:

  pass-by-reference (via RefClasses) integer vector of length
  `num_mesh_vertices`: whether the respective vertex has been visited
  already.

- ref_neighbors:

  pass-by-reference (via RefClasses) integer vector of length
  `num_mesh_vertices`: whether the respective vertex is part of the
  current neighborhood.

- ref_neighbor_dpdists:

  pass-by-reference (via RefClasses) double vector of length
  `num_mesh_vertices`: the dotproduct distance to the respective vertex
  (if it is part of the neighborhood).

## Value

integer, invisible. Either `0L` or `1L`. Used in the recursion only,
ignore. The return values of interest are in the 3 `ref_*` parameters.
