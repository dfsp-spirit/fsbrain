# Get data limiting function, setting values inside range to NA.

Get data limiting function to use in
[`rglactions`](https://dfsp-spirit.github.io/fsbrain/reference/rglactions.md)
as 'trans_fun' to transform data.

## Usage

``` r
limit_fun_na_inside(vmin, vmax)
```

## Arguments

- vmin:

  numerical scalar, the lower border. Data values between this and vmax
  will be set to `NA` in the return value.

- vmax:

  numerical scalar, the upper border. See 'vmin'.

## Value

a function that takes as argument the data, and clips it to the
requested range. I.e., values inside the range will be set to `NA`.
Designed to be used as `rglactions$trans_fun` in vis functions.

## Note

This is useful for thresholding data plotted with a background. All
values inside the range will set to NA and be transparent, and thus be
displayed as the background color.

## See also

[`limit_fun_na`](https://dfsp-spirit.github.io/fsbrain/reference/limit_fun_na.md)
which will set the values outside the range to `NA`.

## Examples

``` r
   rglactions = list("trans_fun"=limit_fun_na_inside(2,3));
```
