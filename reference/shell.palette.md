# Compute the colors and alpha values of the shells.

The palette defines how the shells look: their colors and how
transparent they are, from the outermost to the innermost shell. Three
palettes are available:

- 'grey_context' (the default): all shells but the innermost one are
  grey (like the semi-transparent cortex in
  [`vis.subcortical.region.values`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subcortical.region.values.md)
  is), and the innermost shell is rendered in a warm color and fully
  opaque. This gives the impression of looking at a structure inside a
  translucent head, and it makes a single feature stand out, at the
  price of hiding the level of the inner shells.

- 'sequential': a single hue, getting lighter and more opaque towards
  the innermost shell. Useful to emphasize the nested structure, and to
  avoid the colorful look of 'viridis'.

- 'viridis': the full viridis ramp, with the outer shells dark and very
  transparent and the inner ones bright. This is the most colorful
  option, and it encodes the iso-level in the color.

## Usage

``` r
shell.palette(palette = "grey_context", num_shells = 4L, alpha_range = NULL)
```

## Arguments

- palette:

  character string, the name of the palette: one of 'grey_context',
  'sequential' or 'viridis'.

- num_shells:

  positive integer, the number of shells.

- alpha_range:

  numerical vector of length 2 or `NULL`. The alpha value of the
  outermost and of the innermost (non-opaque) shell. If `NULL`, the
  default range of the palette is used.

## Value

a named list with the entries 'colors' (character vector, one color per
shell) and 'alphas' (numerical vector, one alpha value per shell).
