# Annotate image with text.

Annotate image with text.

## Usage

``` r
images.annotate(images, annotations, do_extend = TRUE, background = "white")
```

## Arguments

- images:

  vector of magick images

- annotations:

  vector of character strings, the strings to print onto the tiles

- do_extend:

  logical, whether to add the space for the annotation text below the
  existing image tile

- background:

  color string, like 'white' or '#00FF00'

## Value

vector of magick images, the annotated images
