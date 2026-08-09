# Return recommended 'makecmap_options' for sequential data with heatmap style.

This function returns recommended visualization settings (a colormap
function and suitable other settings) for the given type of data. The
return value is meant to be passed as parameter 'makecmap_options' to
the vis.\* functions, e.g.,
[`vis.subject.morph.native`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.morph.native.md).

## Usage

``` r
mkco.heat()
```

## Value

named list, visualization settings to be used as 'makecmap_options' for
sequential data with heatmap style.
