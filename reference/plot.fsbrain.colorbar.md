# Draw a simple colorbar from colors.

Draw a simple colorbar from colors.

## Usage

``` r
# S3 method for class 'fsbrain.colorbar'
plot(colors, horizontal = FALSE)
```

## Arguments

- colors:

  vector of colors, no special ordering is assumed

- horizontal:

  logical, whether the colorbar should be plotted horizontally (or
  vertically).

## Note

This function assumes that there is an open plot, use
[`plot.new()`](https://rdrr.io/r/graphics/frame.html) to create one
before calling this function if that is not the case.
