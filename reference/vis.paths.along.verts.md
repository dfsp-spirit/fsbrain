# Visualize several paths in different colors.

Visualize several paths in different colors.

## Usage

``` r
vis.paths.along.verts(
  surface_vertices,
  paths,
  color = viridis::viridis(length(paths))
)
```

## Arguments

- surface_vertices:

  float matrix of size (n, 3), the surface vertex coordinates, as
  returned as part of
  [`subject.surface`](https://dfsp-spirit.github.io/fsbrain/reference/subject.surface.md)
  or
  [`read.fs.surface`](https://rdrr.io/pkg/freesurferformats/man/read.fs.surface.html),
  in the member "vertices". Can also be a
  `freesurferformats::fs.surface` or
  [`rgl::tmesh3d`](https://dmurdoch.github.io/rgl/dev/reference/mesh3d.html)
  instance, in which case the coordinates are extracted automatically.

- paths:

  list of positive integer vectors, the vertex indices of the paths

- color:

  a color string, like '#FF0000' to color the path.
