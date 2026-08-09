# Return recommended 'makecmap_options' for diverging cluster data.

This function returns recommended visualization settings (a colormap
function and suitable other settings) for the given type of data. The
return value is meant to be passed as parameter 'makecmap_options' to
the vis.\* functions, e.g.,
[`vis.subject.morph.native`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.morph.native.md).

## Usage

``` r
mkco.cluster()
```

## Value

named list, visualization settings to be used as 'makecmap_options' for
diverging data.

## Note

This uses a cyan blue red yellow colormap, which is popular for
displaying clusters in neuroscience.
