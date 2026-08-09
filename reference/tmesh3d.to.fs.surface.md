# Get an fs.surface brain mesh from an rgl tmesh3d instance.

Get an fs.surface brain mesh from an rgl tmesh3d instance.

## Usage

``` r
tmesh3d.to.fs.surface(tmesh)
```

## Arguments

- tmesh:

  a tmesh3d instance, see
  [`rgl::tmesh3d`](https://dmurdoch.github.io/rgl/dev/reference/mesh3d.html)
  for details.

## Value

an fs.surface instance, as returned by `subject.surface` or
[`freesurferformats::read.fs.surface`](https://rdrr.io/pkg/freesurferformats/man/read.fs.surface.html).
