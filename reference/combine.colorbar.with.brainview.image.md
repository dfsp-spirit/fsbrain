# Combine a colorbar and a brainview image into a new figure.

Combine a colorbar and a brainview image into a new figure.

## Usage

``` r
combine.colorbar.with.brainview.image(
  brainview_img = "fsbrain_arranged.png",
  colorbar_img = "fsbrain_cbar.png",
  output_img = "fsbrain_merged.png",
  offset = "+0+0",
  extend_brainview_img_height_by = NULL,
  silent = FALSE,
  allow_colorbar_shrink = TRUE,
  horizontal = FALSE,
  background_color = "#FFFFFF",
  transparency_color = NULL,
  delete_colorbar_img = TRUE
)
```

## Arguments

- brainview_img:

  path to the main image containing the view of the brain, usually an
  image in PNG format.

- colorbar_img:

  path to the main image containing the separate colorbar, usually an
  image in PNG format.

- output_img:

  path to output image, including the file extension.

- offset:

  offset string passed to
  [`magick::image_composite`](https://docs.ropensci.org/magick/reference/composite.html).
  Allows you to shift the location of the colorbar in the final image.

- extend_brainview_img_height_by:

  integer value in pixels, the size of the lower border to add to the
  brainview_img. Increase this if the lower part of the colorbar is off
  the image canvas.

- silent:

  logical, whether to silence all messages

- allow_colorbar_shrink:

  logical, whether to shrink the colorbar to the width of the animation
  in case it is considerably wider (more than 20 percent). Defaults to
  TRUE.

- horizontal:

  logical, whether the colorbar is horizontal. If so, it will be added
  below the 'brainview_img'. If it is vertical, it will be added to the
  right of the 'brainview_img'.

- background_color:

  color string, the background color to use. Use 'transparency_color' if
  you want a transparent background.

- transparency_color:

  the temporary background color that will get mapped to transparency,
  or NULL if you do not want a transparent background. If used, it can
  be any color that does not occur in the foreground. Try 'white' or
  'black' if in doubt.

- delete_colorbar_img:

  logical, whether to delete the colorbar_img after the combine
  operation.

## Value

named list with entries 'output_img_path': character string, path to
saved image. 'merged_img': magick image instance, the merged image

## See also

Other colorbar functions:
[`coloredmesh.plot.colorbar.separate()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.plot.colorbar.separate.md),
[`combine.colorbar.with.brainview.animation()`](https://dfsp-spirit.github.io/fsbrain/reference/combine.colorbar.with.brainview.animation.md)
