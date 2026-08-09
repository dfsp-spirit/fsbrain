# Compute expected FWHM from given number of neighborhood smoothing iterations.

Compute expected FWHM from given number of neighborhood smoothing
iterations.

## Usage

``` r
pervertexdata.smoothnn.compute.fwhm(surface, niters, is_template)
```

## Arguments

- surface:

  an `fs.surface` instance

- niters:

  positive integer, the nn iteration count

- is_template:

  logical, whether the surface belongs to a template subject

## Value

double, the expected FWHM

## Note

This function has been adapted from FreeSurfer and it is subject to the
FreeSurfer software license.

## Examples

``` r
if (FALSE) { # \dontrun{
spherical_surface = subject.surface(fsaverage.path(), "fsaverage3", surface="sphere", hemi="lh");
fsbrain:::pervertexdata.smoothnn.compute.fwhm(spherical_surface, 5L);
} # }

```
