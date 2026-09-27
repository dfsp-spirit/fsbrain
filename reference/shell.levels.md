# Compute the iso-levels of the shells.

Compute the iso-levels of the shells.

## Usage

``` r
shell.levels(
  volume,
  levels = NULL,
  num_levels = 5L,
  level_type = "quantile",
  level_range = c(0.2, 0.95)
)
```

## Arguments

- volume:

  a 3D numerical array.

- levels:

  numerical vector or `NULL`, the levels given by the user.

- num_levels:

  positive integer, the number of shells to compute if `levels` is
  `NULL`.

- level_type:

  character string, 'quantile' or 'linear'.

- level_range:

  numerical vector of length 2, the range from which the levels are
  taken.

## Value

ascending numerical vector of levels, the outermost shell first.
