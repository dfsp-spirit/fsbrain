# Get rgloptions and consider global options.

This function retrieves the global rgloptions defined in
`getOption('fsbrain.rgloptions')`, or, if this is not set, returns the
value from
[`rglot`](https://dfsp-spirit.github.io/fsbrain/reference/rglot.md).

## Usage

``` r
rglo()
```

## Value

named list, usable as 'rgloptions' parameter for vis functions like
[`vis.subject.morph.native`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.morph.native.md).

## Note

You can set the default size for all fsbrain figures to 1200x1200 pixels
like this:
`options("fsbrain.rgloptions"=list("windowRect"=c(50,50,1200,1200)))`.
