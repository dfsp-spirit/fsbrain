# Convert integer intensity image to RGB color string form.

Convert a gray-scale image defined by intensity values in range `[0, 1]`
to an image with identical dimensions that contains an R color string
(like `#222222`) at each position. The color strings are computed from
the intensities, by taking the intensity value as the value for all
three RGB channels. I.e., the output is still gray-scale, but defined in
RGB space. To make it clear, this function does **not** apply a
colormap. It only changes the representation of the data, not the
resulting colors.

## Usage

``` r
vol.intensity.to.color(volume, scale = NULL)
```

## Arguments

- volume:

  numeric array, typically a 3D image with intensities in range
  `[0, 1]`. This function now also supports numeric matrices (2D images,
  slices) and numeric vectors (1D).

- scale:

  numeric or character string, a scaling to apply to the values.
  Defaults to NULL, which means *no scaling* and requires the values in
  `volume` to be in rage `[0, 1]`. You can pass a number like 255 or the
  string 'normalize' to scale based on the data. You can pass the string
  'normalize_if_needed' to scale only if the data is *outside* the range
  `[0, 1]`, so that data in range `[0.3, 0.5]` would **not** be rescaled
  to `[0, 1]`.

## Value

array (or matrix, or vector) of RGB color strings. All of them will
represent gray values.

## Examples

``` r
   vol.intensity.to.color(c(0.0, 0.5, 1.0));
#> [1] "#000000" "#808080" "#FFFFFF"
   # output: "#000000" "#808080" "#FFFFFF"
   vol.intensity.to.color(c(20, 186, 240), scale="normalize");
#> [1] "#000000" "#C0C0C0" "#FFFFFF"
   vol.intensity.to.color(c(20, 186, 240), scale=255);
#> [1] "#141414" "#BABABA" "#F0F0F0"
   vol.intensity.to.color(c(0.0, 0.5, 0.8), scale="normalize");
#> [1] "#000000" "#9F9F9F" "#FFFFFF"
   vol.intensity.to.color(c(0.0, 0.5, 0.8), scale="normalize_if_needed");
#> [1] "#000000" "#808080" "#CCCCCC"
```
