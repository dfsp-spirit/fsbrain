# Compute the coordinates of the 8 corners of a 3D box.

Given the extreme values (min and max) along the 3 axes, compute the
coordinates of the 8 corners of a 3D box.

## Usage

``` r
boxcoords.from.bbox(axes_min, axes_max)
```

## Arguments

- axes_min:

  numerical vector of length 3, the min values of the 3 axes

- axes_max:

  numerical vector of length 3, the max values of the 3 axes

## Value

numerical matrix with 3 columns and 8 rows, the edge coordinates
