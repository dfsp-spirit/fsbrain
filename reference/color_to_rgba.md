# Convert an R color to an RGBA float vector

Converts an R color specification (hex string like "#FF0000", color name
like "white", or "#FF0000FF") to a length-4 RGBA numeric vector with
values in the range 0..1.

## Usage

``` r
color_to_rgba(color)
```

## Arguments

- color:

  character string, any valid R color specification.

## Value

numeric vector of length 4, RGBA values in 0..1 range.
