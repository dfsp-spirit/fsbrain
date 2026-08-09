# Compute number of neighborhood smoothing iterations to reach requested fwhm.

Compute number of neighborhood smoothing iterations to reach requested
fwhm.

## Usage

``` r
pervertexdata.smoothnn.compute.numiter(surface, fwhm, is_template)
```

## Arguments

- surface:

  an `fs.surface` instance

- is_template:

  logical, whether the surface belongs to a template subject

## Value

integer, the iteration count

## Note

This function has been adapted from FreeSurfer and it is subject to the
FreeSurfer software license.
