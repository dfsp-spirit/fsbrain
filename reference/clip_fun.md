# Get data clipping function.

Get data clipping function to use in
[`rglactions`](https://dfsp-spirit.github.io/fsbrain/reference/rglactions.md)
as 'trans_fun' to transform data. This is typically used to limit the
colorbar in a plot to a certain range. This uses percentiles to clip.
Clipping means that values more extreme than the gíven quantiles will be
set to the quantile values.

## Usage

``` r
clip_fun(lower = 0.05, upper = 0.95)
```

## Arguments

- lower:

  numeric. The probability for the lower quantile, defaults to `0.05`.

- upper:

  numeric. The probability for the upper quantile, defaults to `0.95`.

## Value

a function that takes as argument the data, and clips it to the
requested range. I.e., values outside the range will be set to the
closest border value. Designed to be used as `rglactions$trans_fun` in
vis functions, to limit the colorbar and data range.

## See also

[`rglactions`](https://dfsp-spirit.github.io/fsbrain/reference/rglactions.md)

## Examples

``` r
   rglactions = list("trans_fun"=clip_fun(0.10, 0.90));
   rglactions = list("trans_fun"=clip_fun());
   f = clip_fun();
   f(rnorm(100));
#>   [1]  0.86208648 -0.24323674 -0.20608719  0.01917759  0.02956075  0.54982754
#>   [7] -1.51785892  1.68306376 -0.36122126  0.21335575  1.07434588 -0.66508825
#>  [13]  1.11395242 -0.24589641 -1.17756331 -0.97585062  1.06505732  0.13167063
#>  [19]  0.48862881 -1.51785892 -1.47073631  0.28415034  1.33732041  0.23669628
#>  [25]  1.31829338  0.52390979  0.60674805 -0.10993567  0.17218172 -0.09032729
#>  [31]  1.68306376  1.29839276  0.74879127  0.55622433 -0.54825726  1.11053489
#>  [37] -1.51785892 -0.15569378  0.43388979 -0.38195111  0.42418757  1.06310200
#>  [43]  1.04871262 -0.03810289  0.48614892  1.67288261 -0.35436116  0.94634789
#>  [49]  1.31682636 -0.29664002 -0.38721358 -0.78543266 -1.05673687 -0.79554143
#>  [55] -1.51785892 -0.69053790 -0.55854199 -0.53666333  0.22712713  0.97845492
#>  [61] -0.20888265 -1.39941046  0.25853729 -0.44179945  0.56859986  1.68306376
#>  [67]  0.42485844 -1.51785892  0.24940178  1.07283825  1.68306376  0.44945378
#>  [73]  1.39181405  0.42656655  0.10758399  0.02229473  0.60361101 -0.26265057
#>  [79] -0.52826408  0.19214942 -1.14619967  0.84618466  0.08171963 -1.30511701
#>  [85] -0.94491206  0.45434159 -0.85520250 -0.28689522  0.89496163  0.06730444
#>  [91] -0.16267634 -0.82731017  1.68306376  0.76644020  0.97995670  1.32178099
#>  [97] -1.11971083  0.51459982 -1.50909984  1.53274148
```
