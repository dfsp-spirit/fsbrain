# Change data to ensure requested data_range.

Change data to ensure requested data_range.

## Usage

``` r
force.to.range(x, data_range, allow_append = FALSE)
```

## Arguments

- x:

  numerical vector, the input data.

- data_range:

  numerical vector of length 2, the range into which to force the data
  values in 'x'. If `NULL`, the input data in 'x' is returned as is.

- allow_append:

  logical, whether to allow adding of more extreme data values. Allows a
  range larger than the data range. If set to `TRUE`, you will have to
  remove the extra values from the colors after generation of the
  colormap.

## Value

Modified version of x. The data will be clamped and / or at most 2
values may be appended to x.

## Note

This is an artificial modification of the data used for plotting a
colormap with a fixed range.
