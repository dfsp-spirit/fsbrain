# Get the glass2 visualization style parameters as a named list.

Glass-brain rendering style. This style has a very negative impact on
rendering performance (especially in interactive mode). Hint: Run
[`material3d`](https://dmurdoch.github.io/rgl/dev/reference/material.html)
without arguments to see valid style keywords to create new styles.

## Usage

``` r
get.rglstyle.glass2()
```

## Value

named list, style parameters that can be passed to
[`shade3d`](https://dmurdoch.github.io/rgl/dev/reference/shade3d.html)
via [`do.call`](https://rdrr.io/r/base/do.call.html).
