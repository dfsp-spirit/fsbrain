# Get the default visualization style parameters as a named list.

Run
[`material3d`](https://dmurdoch.github.io/rgl/dev/reference/material.html)
without arguments to see valid style keywords to create new styles.

## Usage

``` r
get.rglstyle(style)
```

## Arguments

- style:

  string. A style name. Available styles are one of: "default", "shiny",
  "semitransparent", "glass", "edges".

## Value

a style, resolved to a parameter list compatible with
[`material3d`](https://dmurdoch.github.io/rgl/dev/reference/material.html).

## See also

[`shade3d`](https://dmurdoch.github.io/rgl/dev/reference/shade3d.html)
can use the returned style
