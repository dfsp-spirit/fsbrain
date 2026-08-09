# Perform alpha blending for pairs of RGBA colors.

Implements the *over* alpha blending operation.

## Usage

``` r
alphablend(front_color, back_color, silent = TRUE)
```

## Arguments

- front_color:

  rgba color strings, the upper color layer or foreground

- back_color:

  rgba color strings, the lower color layer or background

- silent:

  logical, whether to suppress messages

## Value

rgba color strings, the alpha-blended colors

## References

see the *Alpha blending* section on
https://en.wikipedia.org/wiki/Alpha_compositing

## See also

Other color functions:
[`desaturate()`](https://dfsp-spirit.github.io/fsbrain/reference/desaturate.md)
