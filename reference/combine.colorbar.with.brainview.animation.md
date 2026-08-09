# Combine a colorbar and a brain animation in gif format into a new animation.

Combine a colorbar and a brain animation in gif format into a new
animation.

## Usage

``` r
combine.colorbar.with.brainview.animation(
  brain_animation,
  colorbar_img,
  output_animation,
  offset = "+0+0",
  extend_brainview_img_height_by = 0L,
  silent = FALSE,
  allow_colorbar_shrink = TRUE,
  background_color = "white"
)
```

## Arguments

- brain_animation:

  path to the brain animation in GIF format

- colorbar_img:

  path to the main image containing the separate colorbar, usually an
  image in PNG format

- output_animation:

  path to output image in gif format, must include the '.gif' file
  extension

- offset:

  offset string passed to
  [`magick::image_composite`](https://docs.ropensci.org/magick/reference/composite.html).
  Allows you to shift the location of the colorbar in the final image.

- extend_brainview_img_height_by:

  integer value in pixels, the size of the lower border to add to the
  brainview_img. Use this if the lower part of the colorbar is off the
  image canvas.

- silent:

  logical, whether to silence all messages

- allow_colorbar_shrink:

  logical, whether to shrink the colorbar to the width of the animation
  in case it is considerably wider (more than 20 percent). Defaults to
  TRUE.

- background_color:

  color string, the background color to use. Use 'transparency_color' if
  you want a transparent background.

## See also

Other colorbar functions:
[`coloredmesh.plot.colorbar.separate()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.plot.colorbar.separate.md),
[`combine.colorbar.with.brainview.image()`](https://dfsp-spirit.github.io/fsbrain/reference/combine.colorbar.with.brainview.image.md)
