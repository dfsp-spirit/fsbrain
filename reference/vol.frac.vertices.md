# Compute the vertex coordinates of a (possibly interpolated) surface for one hemisphere.

Compute the vertex coordinates of a (possibly interpolated) surface for
one hemisphere.

## Usage

``` r
vol.frac.vertices(surface, frac_surface, surface_frac)
```

## Arguments

- surface:

  an `fs.surface` instance, the surface to sample, or `NULL` if
  `surface_frac` is given.

- frac_surface:

  an `fs.surface` instance or `NULL`, the second surface for the
  interpolation.

- surface_frac:

  numeric scalar or `NULL`, the fraction from `surface` to
  `frac_surface`.

## Value

numeric matrix, the vertex coordinates.
