# Map an fsbrain rendering style to scimesh render options

Translates fsbrain style names ("default", "shiny", "semitransparent",
"glass", "edges") to scimesh `render_options()` parameters.

## Usage

``` r
fsbrain_style_to_scimesh_options(
  style = "default",
  bg_rgba = c(1, 1, 1, 1),
  width = 800L,
  height = 600L
)
```

## Arguments

- style:

  character string, an fsbrain style name. See
  [`get.rglstyle`](https://dfsp-spirit.github.io/fsbrain/reference/get.rglstyle.md)
  for valid options.

- bg_rgba:

  numeric vector of length 4, the background color in RGBA (0-1 scale).

- width:

  integer, output image width in pixels. Defaults to 800.

- height:

  integer, output image height in pixels. Defaults to 600.

## Value

a scimesh render options list from `render_options()`.
