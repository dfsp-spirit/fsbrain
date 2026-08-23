# Convert hex color string to RGBA float vector

Converts a hex color string like "#FF0000" or "#FF0000FF" to a length-4
RGBA numeric vector with values in the range 0..1.

## Usage

``` r
hex_to_rgba(hex)
```

## Arguments

- hex:

  character string, a hex color code with 6 or 8 digits.

## Value

numeric vector of length 4, RGBA values in 0..1 range.
