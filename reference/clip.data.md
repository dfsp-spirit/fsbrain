# Clip data at quantiles to remove outliers.

Set all data values outside the given quantile range to the border
values. This is useful to properly visualize morphometry data that
includes outliers. These outliers negatively affect the colormap, as all
the non-outlier values become hard to distinguish. This function can be
used to filter the data before plotting it.

## Usage

``` r
clip.data(data, lower = 0.05, upper = 0.95)
```

## Arguments

- data, :

  numeric vector. The input data. Can also be a
  [`hemilist`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.md).

- lower, :

  numeric. The probability for the lower quantile, defaults to `0.05`.

- upper, :

  numeric. The probability for the upper quantile, defaults to `0.95`.

## Value

numeric vector. The output data.

## See also

The
[`clip_fun`](https://dfsp-spirit.github.io/fsbrain/reference/clip_fun.md)
function is more convenient when used in
[`rglactions`](https://dfsp-spirit.github.io/fsbrain/reference/rglactions.md),
as it allows specification of custom quantiles.

## Examples

``` r
   full_data = rnorm(50, 3, 1);
   clipped = clip.data(full_data);
```
