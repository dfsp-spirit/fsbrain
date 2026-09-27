# Compute the highest iso-level per sign.

Computes the highest absolute iso-level for the positive and the
negative clusters, i.e., the level of the innermost shell of a cluster.
If `max_level` is `NULL`, the `max_level_quantile` quantile of the
absolute values beyond the threshold is used, so that a single extreme
voxel does not dominate the visualization.

## Usage

``` r
volume.cluster.extremes(
  volume,
  threshold,
  max_level = NULL,
  max_level_quantile = 0.99,
  positive = TRUE,
  negative = TRUE
)
```

## Arguments

- volume:

  a 3D numerical array.

- threshold:

  single positive number, the cluster threshold.

- max_level:

  single positive number or `NULL`, the highest level given by the user.

- max_level_quantile:

  single value between 0 and 1, the quantile used to compute the highest
  level if `max_level` is `NULL`.

- positive:

  logical, whether to consider the positive clusters.

- negative:

  logical, whether to consider the negative clusters.

## Value

named numerical vector with the entries 'neg' and 'pos' (the highest
absolute iso-level of the negative and of the positive clusters), or
`NA` for a sign that has no voxels beyond the threshold (or was not
requested).
