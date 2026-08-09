# Set default figure size for fsbrain visualization functions.

Set default figure size for fsbrain visualization functions.

## Usage

``` r
fsbrain.set.default.figsize(width, height, xstart = 50L, ystart = 50L)
```

## Arguments

- width:

  integer, default figure width in pixels

- height:

  integer, default figure height in pixels

- xstart:

  integer, default horizontal position of plot window on screen, left
  border is 0. The max value (right border) depends on your screen
  resolution.

- ystart:

  integer, default vertical position of plot window on screen, upper
  border is 0. The max value (lower border) depends on your screen
  resolution.

## Note

This function overwrites `options("fsbrain.rgloptions")`. Output size is
limited by your screen resolution. To set your preferred figure size for
future R sessions, you could call this function in your `'~/.Rprofile'`
file.
