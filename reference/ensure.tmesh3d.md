# Ensure the mesh is a tmesh3d instance. Will convert fs.surfaces to one automatically.

Ensure the mesh is a tmesh3d instance. Will convert fs.surfaces to one
automatically.

## Usage

``` r
ensure.tmesh3d(mesh)
```

## Arguments

- mesh:

  whatever, but hopefully an
  [`rgl::tmesh3d`](https://dmurdoch.github.io/rgl/dev/reference/mesh3d.html)
  or `freesurferformats::fs.surface` instance. Can be a character
  string, which will be loaded as a surface file if it exists.

## Value

tmesh3d instance, the input or converted from the input.

## Note

This function will stop if the mesh cannot be converted to tmesh3d.
