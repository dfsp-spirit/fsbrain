# Get an rgl tmesh3d instance from a brain surface mesh.

Get an rgl tmesh3d instance from a brain surface mesh.

## Usage

``` r
fs.surface.to.tmesh3d(surface)
```

## Arguments

- surface:

  an fs.surface instance, as returned by `subject.surface` or
  [`freesurferformats::read.fs.surface`](https://rdrr.io/pkg/freesurferformats/man/read.fs.surface.html).

## Value

a tmesh3d instance, see
[`rgl::tmesh3d`](https://dmurdoch.github.io/rgl/dev/reference/mesh3d.html)
for details.
