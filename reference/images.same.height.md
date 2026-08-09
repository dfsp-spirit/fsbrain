# Extent all images to the height of the image with maximal height.

Extent all images to the height of the image with maximal height.

## Usage

``` r
images.same.height(images, background_color = "white")
```

## Arguments

- images:

  a vector/stack of magick images. See
  [`magick::image_blank`](https://docs.ropensci.org/magick/reference/editing.html)
  or other methods to get one.

- background_color:

  hex color string, the background color to use if the images have
  different sizes and one needs to be extended. Do not use color names
  like 'gray', which differ between R and magick.

## Value

a vector/stack of magick images, all with the same height.
