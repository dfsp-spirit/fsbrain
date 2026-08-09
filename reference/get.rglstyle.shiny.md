# Get a shiny visualization style.

A shiny or glossy rendering style. Looks a bit more modern, but the
resulting highlights may make the interpretation of the plotted data a
bit harder in some areas. Hint: Run
[`material3d`](https://dmurdoch.github.io/rgl/dev/reference/material.html)
without arguments to see valid style keywords to create new styles.

## Usage

``` r
get.rglstyle.shiny()
```

## Value

named list, style parameters that can be passed to to
[`shade3d`](https://dmurdoch.github.io/rgl/dev/reference/shade3d.html)
via [`do.call`](https://rdrr.io/r/base/do.call.html).
