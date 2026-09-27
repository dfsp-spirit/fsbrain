# Compute the cluster threshold of a volume.

Computes the threshold from the data if none was given, namely the
requested quantile of the absolute values of the non-zero voxels.

## Usage

``` r
volume.cluster.threshold(volume, threshold = NULL, threshold_quantile = 0.95)
```

## Arguments

- volume:

  a 3D numerical array.

- threshold:

  single positive number or `NULL`, the threshold given by the user.

- threshold_quantile:

  single value between 0 and 1, the quantile used if `threshold` is
  `NULL`.

## Value

single positive number, the threshold.
