# Create rglactions list, suitable to be passed as parameter to vis functions.

Create rglactions list, suitable to be passed as parameter to vis
functions.

## Usage

``` r
rglactions()
```

## Value

named list, an example `rlgactions` instance that will save a screenshot
of the plot produced by the vis function in the current working
directory (see `getwd`), under the name 'fsbrain_out.png'.

## Note

List of all available rglactions: (1) `snapshot_png=filepath` takes a
screenshot in PNG format and saves it in at `filepath`. (2)
`trans_fun=function` uses the transformation function trans_fun to the
data before mapping data values to colors and plotting. Popular
transformation functions are
[`limit_fun`](https://dfsp-spirit.github.io/fsbrain/reference/limit_fun.md),
[`limit_fun_na`](https://dfsp-spirit.github.io/fsbrain/reference/limit_fun_na.md),
and
[`clip_fun`](https://dfsp-spirit.github.io/fsbrain/reference/clip_fun.md).
(3) `text=arglist` calls
[`text3d`](https://dmurdoch.github.io/rgl/dev/reference/texts.html) with
the given args after plotting. (4) `snapshot_vec=filepath` takes a
screenshot in vector format and saves it in at `filepath`. You also need
to set the format via `snapshot_vec_format`, valid entries are one of
"ps", "eps", "tex", "pdf", "svg", "pgf" (default is 'eps'). This is
experimental and may take a while.

## Examples

``` r
   rgla_screenie = list('snapshot_png'='fsbain_out.png');
   rgla_screenie = rglactions();   # same as above
   rgla_vec_scr = list('snapshot_vec'="~/fsbrain.pdf",
     "snapshot_vec_format"="pdf");
   rgla_clamp = list('trans_fun'=clip.data); # old style
   rgla_clamp = list('trans_fun'=clip_fun(0.05, 0.95)); # new style
   rgla_clamp = list('trans_fun'=clip_fun());            # equivalent.
   rgla_limit = list('trans_fun'=limit_fun(2,5));
   rgla_ls = list('trans_fun'=limit_fun_na(2,5), 'snapshot_png'='~/fig1.png');
```
