# Generate test 3D volume of integers. The volume has an outer background area (intensity value 'bg') and an inner foreground areas (intensity value 200L).

Generate test 3D volume of integers. The volume has an outer background
area (intensity value 'bg') and an inner foreground areas (intensity
value 200L).

## Usage

``` r
gen.test.volume(vdim = c(256L, 256L, 256L), bg = NA)
```

## Arguments

- vdim:

  integer vector of length 3, the dimensions

- bg:

  value to use for outer background voxels. Typically `0L` or `NA`.

## Value

a 3d array of integers

## Note

This function exists for software testing purposes only, you should not
use it in client code.
