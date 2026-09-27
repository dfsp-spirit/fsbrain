# Cut away part of a mesh.

Removes all faces of a mesh which are (partially) located in the given
anatomical direction of a cut plane, so that one can look inside the
object. The mesh is not closed at the cut plane.

## Usage

``` r
shell.cut.away(mesh, cut_away = "right", cut_fraction = 0.5)
```

## Arguments

- mesh:

  a `mesh3d` instance, in surface RAS coordinates.

- cut_away:

  character string, one of 'left', 'right', 'posterior', 'anterior',
  'inferior' or 'superior'.

- cut_fraction:

  numerical, the position of the cut plane along the respective axis, as
  a fraction of the bounding box of the mesh.

## Value

a `mesh3d` instance with the respective faces removed.
