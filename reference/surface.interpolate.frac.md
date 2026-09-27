# Compute the vertex positions at a given fraction between two surfaces.

Computes a new mesh whose vertex positions lie on the line between the
corresponding vertices of two meshes with identical vertex counts, e.g.,
between the white and the pial surface. This is what the FreeSurfer
command line option `--surf-frac` does.

## Usage

``` r
surface.interpolate.frac(surface_a, surface_b, frac)
```

## Arguments

- surface_a:

  an `fs.surface` instance, the surface at fraction 0.

- surface_b:

  an `fs.surface` instance, the surface at fraction 1. Must have the
  same number of vertices as `surface_a`.

- frac:

  numeric scalar, the fraction, in range 0..1.

## Value

numeric matrix of vertex coordinates (see `surface_a$vertices`).
