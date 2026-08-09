# Apply spatial filter to surface data.

Apply spatial filter to surface data.

## Usage

``` r
surf.sphere.spatialfilter(source_data, sphere_dists, gaussian_weights)
```

## Arguments

- source_data:

  numerical vector, per-vertex data for a surface.

- sphere_dists:

  named list with 3 entries, as returned by `surf.sphere.dist`

- gaussian_weights:

  list of double vectors, the Gaussian weights for all neighbors of the
  respective vertex. As returned by `surf.sphere.gaussianweights`.

## Value

numerical vector, the spatially filtered per-vertex data.
