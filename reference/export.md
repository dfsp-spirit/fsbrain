# Export high-quality brainview image with a colorbar.

This function serves as an easy (but slightly inflexible) way to export
a high-quality, tight-layout, colorbar figure to disk. If no colorbar is
required, one can use `vislayout.from.coloredmeshes` instead. It is an
alias for `vis.export.from.coloredmeshes` that requires less typing.

## Usage

``` r
export(
  coloredmeshes,
  colorbar_legend = NULL,
  img_only = TRUE,
  draw_colorbar = "horizontal",
  horizontal = NULL,
  silent = TRUE,
  quality = 1L,
  output_img = "fsbrain_arranged.png",
  image.plot_extra_options = NULL,
  large_legend = TRUE,
  view_angles = get.view.angle.names(angle_set = "t4"),
  style = "default",
  grid_like = TRUE,
  background_color = "white",
  transparency_color = NULL,
  ...
)
```

## Arguments

- coloredmeshes:

  list of coloredmesh. A coloredmesh is a named list as returned by the
  `coloredmesh.from*` functions (like
  [`coloredmesh.from.morph.native`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.morph.native.md)).
  It has the entries 'mesh' of type tmesh3d, a 'col', which is a color
  specification for such a mesh. The `vis*` functions (like
  [`vis.subject.morph.native`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.morph.native.md))
  all return a list of coloredmeshes.

- colorbar_legend:

  character string or NULL, the title for the colorbar.

- img_only:

  logical, whether to return only the resulting image

- draw_colorbar:

  logical or one of the character strings 'vertical' or 'horizontal',
  whether to draw a colorbar. Defaults to 'horizontal'.

- horizontal:

  deprecated (since 0.5.0) and ignored, use parameter 'draw_colorbar'
  instead.

- silent:

  logical, whether to suppress messages

- quality:

  integer, an arbitrary quality. This is the resolution per tile before
  trimming, divided by 1000, in pixels. Example: 1L means 1000x1000
  pixels per tile before trimming. Currently supported values: `1L..2L`.
  Note that the resolution you can get is also limited by your screen
  resolution.

- output_img:

  string, path to the output file. Defaults to "fsbrain_arranged.png"

- image.plot_extra_options:

  named list, custom options for fields::image.plot. Overwrites those
  derived from the quality setting. If in doubt, leave this alone.

- large_legend:

  logical, whether to plot extra large legend text, affects the font
  size of the colorbar_legend and the tick labels.

- view_angles:

  list of strings. See
  [`get.view.angle.names`](https://dfsp-spirit.github.io/fsbrain/reference/get.view.angle.names.md)
  for all valid strings.

- style:

  the rendering style, see `material3d` or use a predefined style like
  'default' or 'shiny'.

- grid_like:

  logical, passed to `vislayout.from.coloredmeshes`.

- background_color:

  hex color string (like '#FFFFFF'), the color to use for the
  background. Ignored if 'transparency_color' is not NULL. To get a
  transparent background, use 'transparency_color' instead of this
  parameter. WARNING: Do not use color names (like 'gray'), as their
  interpretation differs between rgl and image magick!

- transparency_color:

  hex color string (like '#FFFFFF'), the temporary background color that
  will get mapped to transparency, or NULL if you do not want a
  transparent background. If used, it can be any color that does not
  occur in the foreground. Try '#FFFFFF' (white) or '#000000' (black) if
  in doubt. WARNING: Do not use color names (like 'gray'), as their
  interpretation differs between rgl and image magick!

- ...:

  extra arguments passed to `vislayout.from.coloredmeshes`.

## Value

magick image instance or named list, depending on the value of
'img_only'. If the latter, the list contains the fields 'rev_vl',
'rev_cb', and 'rev_ex', which are the return values of the functions
`vislayout.from.coloredmeshes`, `coloredmesh.plot.colorbar.separate`,
and `combine.colorbar.with.brainview.image`, respectively.

## Note

Note that your screen resolution has to be high enough to generate the
final image in the requested resolution, see the 'fsbrain FAQ' vignette
for details and solutions if you run into trouble.

## Examples

``` r
if (FALSE) { # \dontrun{
    rand_data = rnorm(327684, 5, 1.5);
    cm = vis.data.on.fsaverage(morph_data_both=rand_data,
      rglactions=list('no_vis'=T));
    export(cm, colorbar_legend='Random data',
      output_img="~/fsbrain_arranged.png");
} # }
```
