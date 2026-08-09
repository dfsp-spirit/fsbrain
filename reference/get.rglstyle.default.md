# Get the default visualization style parameters as a named list.

The default rendering style, which is is rather plain. Does not look
super fancy, but allows for clear data visualization without
distractions. Hint: Run
[`material3d`](https://dmurdoch.github.io/rgl/dev/reference/material.html)
without arguments to see valid style keywords to create new styles.

## Usage

``` r
get.rglstyle.default()
```

## Value

named list, style parameters that can be passed to
[`shade3d`](https://dmurdoch.github.io/rgl/dev/reference/shade3d.html)
via [`do.call`](https://rdrr.io/r/base/do.call.html).
