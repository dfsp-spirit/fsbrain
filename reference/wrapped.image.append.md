# Wrapper around magick::image_append that allows specifying the background color when working with images of different width/height.

Wrapper around magick::image_append that allows specifying the
background color when working with images of different width/height.

## Usage

``` r
wrapped.image.append(images, stack = FALSE, background_color = "#FFFFFF")
```

## Arguments

- images:

  a vector/stack of magick images. See
  [`magick::image_blank`](https://docs.ropensci.org/magick/reference/editing.html)
  or other methods to get one.

- stack:

  whether to append vertically, default is FALSE / horizontally.

- background_color:

  hex color string, the background color to use if the images have
  different sizes and one needs to be extended. Do not use color names
  like 'gray', which differ between R and magick.

## Value

a single magick image, the stacked version.
