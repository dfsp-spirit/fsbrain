# Perform simple desaturation or grayscale conversion of RGBA colors.

Perform simple desaturation or grayscale conversion of RGBA colors.

## Usage

``` r
desaturate(color, gamma_correct = FALSE)
```

## Arguments

- color:

  rgba color strings

- gamma_correct:

  logical, whether to apply non-linear gamma correction. First performs
  gamma expansion, then applies the gray-scale channel weights, then
  gamma compression.

## Value

rgba color strings, the gray-scale colors. The information from one of
the three RGB channels would be enough. The alpha value is not touched.

## Note

Assumes sRGB color space.

## References

see
https://en.wikipedia.org/wiki/Grayscale#Converting_color_to_grayscale

## See also

Other color functions:
[`alphablend()`](https://dfsp-spirit.github.io/fsbrain/reference/alphablend.md)
