# Combine a vertical colorbar and a brainview image into a new figure.

Combine a vertical colorbar and a brainview image into a new figure.

## Usage

``` r
combine.colorbar.with.brainview.image.vertical(
  brainview_img,
  colorbar_img,
  output_img,
  offset = "+0+0",
  extend_brainview_img_width_by = NULL,
  silent = FALSE,
  allow_colorbar_shrink = TRUE,
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

- extend_brainview_img_width_by:

  integer value in pixels, the size of the right border to add to the
  brainview_img. Increase this if the right part of the colorbar is off
  the image canvas.

- silent:

  logical, whether to silence all messages

- allow_colorbar_shrink:

  logical, whether to shrink the colorbar to the width of the animation
  in case it is considerably wider (more than 20 percent). Defaults to
  TRUE.

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
