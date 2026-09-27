# Smooth a volume with a 3x3x3 box blur.

Applies a separable 3x3x3 box blur to a 3D array, once per requested
pass. Voxels at the border of the volume are treated as if the volume
were padded with its edge values.

## Usage

``` r
volume.boxblur(volume, passes = 1L)
```

## Arguments

- volume:

  a 3D numerical array.

- passes:

  positive integer, the number of blur passes.

## Value

a 3D numerical array with identical dimensions.
