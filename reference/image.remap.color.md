# Remap a color in an image, typically used to set the background color to transparent.

Offers 2 algorithm: remap color by flood-filling from a given pixel, or
remap a hardcoded color throughout the entire image. Provide one of
'source_color' or 'source_point' by setting the other to NULL. If both
are given, source_color takes precedence and source_point is silently
ignored.

## Usage

``` r
# S3 method for class 'remap.color'
image(
  source_img,
  source_color = NULL,
  source_point = "+1+1",
  target_color = "none"
)
```

## Arguments

- source_color:

  the source color that should be replaced in the whole image. Set to
  NULL to disable.

- source_point:

  the source pixel in which to start the flood filling. Set to NULL to
  disable.

- target_color:

  an image magick color string, use 'none' for transparency. Only used
  with flood fill.
