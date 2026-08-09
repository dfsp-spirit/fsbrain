# Get data limiting function.

Get data limiting function to use in rglactions as 'trans_fun' to
transform data. This is typically used to limit the colorbar in a plot
to a certain range. This is similar to
[`clip.data`](https://dfsp-spirit.github.io/fsbrain/reference/clip.data.md)
or
[`clip_fun`](https://dfsp-spirit.github.io/fsbrain/reference/clip_fun.md),
but uses absolute values instead of percentiles to clip.

## Usage

``` r
limit_fun(vmin, vmax)
```

## Arguments

- vmin:

  numerical scalar, the lower border. Data values below this will be set
  to vmin in the return value.

- vmax:

  numerical scalar, the upper border. Data values above this will be set
  to vmax in the return value.

## Value

a function that takes as argument the data, and clips it to the
requested range. I.e., values outside the range will be set to the
closest border value ('vmin' or 'vmax'). Designed to be used as
`rglactions$trans_fun` in vis functions, to limit the colorbar and data
range.

## See also

[`rglactions`](https://dfsp-spirit.github.io/fsbrain/reference/rglactions.md)

## Examples

``` r
   rglactions = list("trans_fun"=limit_fun(2,3));
```
