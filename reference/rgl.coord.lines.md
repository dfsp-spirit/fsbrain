# Plot x, y and z axes in R,G,B.

Plot positive x, y, and z axes from the center to 'len'. Gets added to
current plot. Useful for debugging and understanding the `rgl`
coordinate system.

## Usage

``` r
rgl.coord.lines(len = 100)
```

## Arguments

- len:

  numeric scalar or vector of length 3, length of axes. You can specify
  a negative value to see the negative directions.

## Value

`NULL`, called for the plotting side effect.

## Note

The x, y and z axes are plotted in red, green, and blue, respectively.
