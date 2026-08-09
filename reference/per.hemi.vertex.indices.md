# Transform surfaces indices which go over two surfaces to per-hemi indices.

Transform surfaces indices which go over two surfaces to per-hemi
indices.

## Usage

``` r
per.hemi.vertex.indices(surfaces, vertices)
```

## Arguments

- surfaces:

  hemilist of surfaces, or a single integer, which will be interpreted
  as the number of vertices of the left hemisphere surface.

- vertices:

  positive integer vector, the query vertex indices. Must be in range 1
  to (num_verts_lh + num_verts_rh).

## Value

named list with the following entries: 'vertices', hemilist of indices
for the hemispheres. 'query_indices', hemilist of the indices of the
respective vertices in the vector that was passed as parameter vertices.
'vertices_hemi': vector of character strings containing the hemi value
(lh or rh) for the query vertices.
