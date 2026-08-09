# Combine several brainview images into a new figure.

Create a new image from several image tiles, the exact layout depends on
the number of given images.

## Usage

``` r
arrange.brainview.images(
  brainview_images,
  output_img,
  colorbar_img = NULL,
  silent = TRUE,
  grid_like = TRUE,
  border_geometry = "5x5",
  background_color = "white",
  map_bg_to_transparency = FALSE
)
```

## Arguments

- brainview_images:

  vector of character strings, paths to the brainview images, usually in
  PNG format

- output_img:

  path to output image that including the file extension

- colorbar_img:

  path to the main image containing the separate colorbar, usually an
  image in PNG format

- silent:

  logical, whether to suppress messages

- grid_like:

  logical, whether to arrange the images in a grid-like fashion. If
  FALSE, they will all be merged horizontally.

- border_geometry:

  string, a geometry string passed to
  [`magick::image_border`](https://docs.ropensci.org/magick/reference/composite.html)
  to define the borders to add to each image tile. The default value
  adds 5 pixels, both horizontally and vertically.

- background_color:

  hex color string, such as "#DDDDDD" or "#FFFFFF". The color to use
  when extending images (e.g., when creating the border). WARNING: Do
  not use color names (like 'gray'), as their interpretation differs
  between rgl and image magick!

- map_bg_to_transparency:

  logical, whether to map the background_color to transparency for the
  final PNG export.

## Value

named list with entries: 'brainview_images': vector of character
strings, the paths to the input images. 'output_img_path': character
string, path to the output image. 'merged_img': the magick image
instance.
