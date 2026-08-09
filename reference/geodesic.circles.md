# Compute geodesic circles and ball stats for given vertices.

Compute geodesic circles and ball stats for given vertices.

## Usage

``` r
geodesic.circles(surface, vertices = NULL, scale = 5)
```

## Arguments

- surface:

  an
  [`rgl::tmesh3d`](https://dmurdoch.github.io/rgl/dev/reference/mesh3d.html)
  or `freesurferformats::fs.surface` instance. Can be a character
  string, which will be loaded as a surface file if it exists.

- vertices:

  positive integer vector, the vertex indices for which to compute the
  stats. If NULL, it is computed for all vertices.

- scale:

  double, surface area to be covered by patch in percent

## Note

This takes a while for large meshes, try it with single vertices or with
a surface like fsaverage3 if you want it for all vertices. This requires
the optional dependency package 'pracma'.

## Examples

``` r
if (FALSE) { # \dontrun{
  sjd = fsaverage.path(TRUE);
  surface = subject.surface(sjd, 'fsaverage3', hemi='lh');
  gc = geodesic.circles(surface);
  vis.data.on.subject(sjd, 'fsaverage3', morph_data_lh = gc$radius);
  vis.data.on.subject(sjd, 'fsaverage3', morph_data_lh = gc$perimeter);
} # }
```
