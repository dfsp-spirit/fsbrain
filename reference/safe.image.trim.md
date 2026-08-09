# Safe wrapper around magick::image_trim that handles blank images.

Safe wrapper around magick::image_trim that handles blank images.

## Usage

``` r
safe.image.trim(image)
```

## Arguments

- image:

  a magick image object.

## Value

the trimmed magick image, or the original if trimming fails (e.g.,
uniform/blank image).
