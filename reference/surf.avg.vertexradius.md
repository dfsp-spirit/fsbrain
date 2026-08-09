# Compute average distance from the origin to each vertex.

Compute average distance from the origin to each vertex.

## Usage

``` r
surf.avg.vertexradius(surface, with_stddev = FALSE)
```

## Arguments

- surface:

  an fs.surface instance, and for the typical use case of this function,
  a spherical surface.

- with_stddev:

  logical, whether to compute the standard deviation as well and return
  a named list with the 'avg' and the 'stddev' instead.

## Value

scalar double, the average distance.

## Note

This is used to determine the sphere radius for spherical surfaces. It
is assumed that the sphere is centered at the origin `(0,0,0)`.

## Examples

``` r
if (FALSE) { # \dontrun{
spherical_surface = subject.surface(fsaverage.path(), "fsaverage3",
  surface="sphere", hemi="lh");
vr = fsbrain:::surf.avg.vertexradius(spherical_surface);
# Show histogram to verify that the surface is a sphere centered at 0,0,0:
hist(freesurferformats::vertexdists.to.point(spherical_surface, c(0,0,0)));
# Plot the coords and a point at the origin:
fsbrain::highlight.points.spheres(rbind(spherical_surface$vertices, c(0,0,0)));
} # }
```
