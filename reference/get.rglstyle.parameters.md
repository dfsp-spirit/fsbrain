# Produce the named list of style parameters from style definition.

A style definition can be a character string like "shiny", already a
parameter list, or a command like 'from_mesh' that tells us to get the
style from the renderable. This function creates the final parameters
from the definition and the renderable.

## Usage

``` r
get.rglstyle.parameters(renderable, style)
```

## Arguments

- renderable:

  A renderable (or any list) which includes a 'style' key. If it does
  not include such a key, the 'default' style will be used.

- style:

  A style definition. Can be a character string like 'shiny' or
  'from_mesh', or already a named list of material properties (which
  will be returned as-is).

## Value

a style, resolved to a parameter list compatible with
[`material3d`](https://dmurdoch.github.io/rgl/dev/reference/material.html).
