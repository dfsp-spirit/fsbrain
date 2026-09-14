# Convert a misc3d Triangles3D iso-surface to a coloredmesh.

Convert a `misc3d::contour3d(draw = FALSE)` result (an iso-surface mesh
of class 'Triangles3D', e.g., as returned by
[`volvis.contour`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.contour.md))
into an `fs.coloredmesh`. This allows rendering volume iso-surfaces with
ANY renderer backend: while the rgl backend can render 'Triangles3D'
instances directly, the scimesh backend requires `fs.coloredmesh`
instances and converts them automatically, so you normally do not need
to call this function yourself.

## Usage

``` r
Triangles3D.to.coloredmesh(tris, hemi = NULL, add_normals = TRUE)
```

## Arguments

- tris:

  a 'Triangles3D' instance as returned by
  `misc3d::contour3d(draw = FALSE)` or
  [`volvis.contour`](https://dfsp-spirit.github.io/fsbrain/reference/volvis.contour.md),
  or a list of such instances (e.g., when multiple frames of a 4D volume
  were extracted with `frame = "all"`).

- hemi:

  character string or NULL, the hemisphere for the resulting
  coloredmesh. 'Triangles3D' iso-surfaces are not hemisphere-specific,
  so this defaults to NULL. See
  [`fs.coloredmesh`](https://dfsp-spirit.github.io/fsbrain/reference/fs.coloredmesh.md).

- add_normals:

  logical, whether to compute per-vertex normals for the mesh. Required
  for correct lighting in the scimesh backend, defaults to TRUE.

## Value

an `fs.coloredmesh` instance, or a list of such instances if 'tris' is a
list.

## See also

Other coloredmesh functions:
[`coloredmesh.from.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.annot.md),
[`coloredmesh.from.label()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.label.md),
[`coloredmesh.from.mask()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.mask.md),
[`coloredmesh.from.morph.native()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.morph.native.md),
[`coloredmesh.from.morph.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.morph.standard.md),
[`coloredmesh.from.morphdata()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.morphdata.md),
[`coloredmeshes.from.color()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmeshes.from.color.md)
