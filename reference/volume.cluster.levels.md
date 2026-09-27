# Compute the iso-levels of the cluster shells.

Computes the iso-levels at which the shells of the clusters are
extracted: `num_levels` levels from the threshold up to the highest
level, for each requested sign, ordered from the outermost (the
threshold) to the innermost (the highest level) shell.

## Usage

``` r
volume.cluster.levels(
  volume,
  threshold,
  num_levels = 10L,
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

- num_levels:

  positive integer, the number of levels per sign.

- max_level:

  single positive number or `NULL`, the highest level. If `NULL`, it is
  computed per sign as the `max_level_quantile` quantile of the absolute
  values beyond the threshold.

- max_level_quantile:

  single value between 0 and 1, the quantile used to compute `max_level`
  if that is `NULL`.

- positive:

  logical, whether to compute levels for the positive side.

- negative:

  logical, whether to compute levels for the negative side.

## Value

numerical vector of iso-levels, the negative ones (if any) first, each
ordered from the outermost to the innermost shell.
