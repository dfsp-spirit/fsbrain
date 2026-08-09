# fs.coloredmesh constructor

fs.coloredmesh constructor

## Usage

``` r
fs.coloredmesh(
  mesh,
  col,
  hemi,
  render = TRUE,
  metadata = NULL,
  add_normals = FALSE
)
```

## Arguments

- mesh:

  a `mesh3d` instance as returned by
  [`tmesh3d`](https://dmurdoch.github.io/rgl/dev/reference/mesh3d.html)
  or an `fs.surface` brain surface mesh as returned by functions like
  [`subject.surface`](https://dfsp-spirit.github.io/fsbrain/reference/subject.surface.md).

- col:

  vector of vertex colors for the mesh, one color per vertex. Expanded
  if exactly one color.

- hemi:

  character string, one of 'lh' or 'rh'. This may be used by
  visualization functions to decide whether or not to show this mesh in
  a certain view.

- render:

  logical, whether to render this mesh during visualization

- metadata:

  optional, named list containing metadata

- add_normals:

  logical, whether to compute normals and save them in the mesh.

## Value

an `fs.coloredmesh` instance. The only fields one should use in client
code are 'mesh', 'hemi' and 'col', all others are considered internal
and may change without notice.
