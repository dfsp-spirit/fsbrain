# Compute the alpha value of a cluster shell.

The shells of a cluster are more transparent the closer they are to the
threshold, and fully opaque at the highest level of their sign.

## Usage

``` r
cluster.shell.alpha(level, threshold, levels, alpha_range = c(0.08, 1))
```

## Arguments

- level:

  single number, the iso-level of the shell.

- threshold:

  single positive number, the cluster threshold.

- levels:

  numerical vector of all iso-levels, see
  [`volume.cluster.levels`](https://dfsp-spirit.github.io/fsbrain/reference/volume.cluster.levels.md).

- alpha_range:

  numerical vector of length 2, the alpha value of the outermost and the
  innermost shell.

## Value

single number between 0 and 1, the alpha value of the shell.
