# Combine several brainview images as a grid into a new figure.

Create a new image from several image tiles, the exact layout is a grid
with n per row.

## Usage

``` r
arrange.brainview.images.grid(
  brainview_images,
  output_img,
  colorbar_img = NULL,
  silent = TRUE,
  num_per_row = 10L,
  border_geometry = "5x5",
  background_color = "white",
  captions = NULL
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

- num_per_row:

  positive integer, the number of image tiles per row.

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

- captions:

  vector of character strings or NULL, the (optional) text annotations
  for the images. Useful to print the subject identifier onto the
  individual tiles. Length must match number of image tiles in
  'brainview_images'.

## Value

named list with entries: 'brainview_images': vector of character
strings, the paths to the input images. 'output_img_path': character
string, path to the output image. 'merged_img': the magick image
instance.

## Note

The tiles are written row-wise, in the order in which they occur in the
parameter 'brainview_images'.
