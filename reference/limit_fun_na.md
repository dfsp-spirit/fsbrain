# Get data limiting function to NA.

Get data limiting function to use in
[`rglactions`](https://dfsp-spirit.github.io/fsbrain/reference/rglactions.md)
as 'trans_fun' to transform data. This is typically used to limit the
colorbar in a plot to a certain range. This is similar to
[`clip.data`](https://dfsp-spirit.github.io/fsbrain/reference/clip.data.md),
but uses absolute values instead of percentiles to clip.

## Usage

``` r
limit_fun_na(vmin, vmax)
```

## Arguments

- vmin:

  numerical scalar, the lower border. Data values below this will be set
  to `NA` in the return value.

- vmax:

  numerical scalar, the upper border. Data values above this will be set
  to `NA` in the return value.

## Value

a function that takes as argument the data, and clips it to the
requested range. I.e., values outside the range will be set to `NA`.
Designed to be used as `rglactions$trans_fun` in vis functions, to limit
the colorbar and data range.

## Note

This is useful for thresholding stuff like t-value maps. All values
outside the range will be displayed as the background color.

## See also

[`limit_fun_na_inside`](https://dfsp-spirit.github.io/fsbrain/reference/limit_fun_na_inside.md)
which will set the values inside the range to `NA`.

## Examples

``` r
   rglactions = list("trans_fun"=limit_fun_na(2,3));
```
