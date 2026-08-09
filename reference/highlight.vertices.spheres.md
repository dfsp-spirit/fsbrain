# Draw small 3D spheres at given brain mesh vertices. Supports full brain (2 meshes) as well.

Draw small 3D spheres at given brain mesh vertices. Supports full brain
(2 meshes) as well.

## Usage

``` r
highlight.vertices.spheres(surface, vertices, ...)
```

## Arguments

- surface:

  an fs.surface instance, see
  [`subject.surface`](https://dfsp-spirit.github.io/fsbrain/reference/subject.surface.md)
  function. Can also be a hemilist of surfaces, in which case the
  vertices can be indices over both meshes (in range
  `1..(nv(lh)+nv(rh))`).

- vertices:

  vector of positive integers, the vertex indices. Values which are
  outside of the valid indices for the surface will be silently ignored,
  making it easier to work with the two hemispheres.

- ...:

  Parameters passed to
  [`highlight.points.spheres`](https://dfsp-spirit.github.io/fsbrain/reference/highlight.points.spheres.md).

## Note

This function will draw into the current window and add to the scene, so
it can be called after visualizing a mesh. See the example.

## See also

Other 3d utility functions:
[`highlight.points.spheres()`](https://dfsp-spirit.github.io/fsbrain/reference/highlight.points.spheres.md),
[`vertex.coords()`](https://dfsp-spirit.github.io/fsbrain/reference/vertex.coords.md)

## Examples

``` r
if (FALSE) { # \dontrun{
lh_surf = subject.surface('~/data/study1', 'subject1',
 surface = "white", hemi = "lh");
vis.fs.surface(lh_surf, style="semitransparent");
highlight.vertices.spheres(lh_surf,
  vertices = c(3225L, 4300L, 5500L),
  color = c("green", "blue", "red"));
} # }
```
