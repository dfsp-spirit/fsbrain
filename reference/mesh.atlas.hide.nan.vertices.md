# Hide the vertices that carry NaN data in a coloredmesh.

Remove the vertices (and all faces that use them) for which the data
value is NaN. This hides the respective region completely, it is not
rendered at all. This is a per-vertex operation, so it also works for
meshes in which several regions share a single mesh (like a mesh atlas),
in contrast to the mesh-wide rendering style. It is the basis of hiding
individual regions of a mesh atlas by passing NaN as their value.

## Usage

``` r
mesh.atlas.hide.nan.vertices(cmesh, hemi)
```

## Arguments

- cmesh:

  fs.coloredmesh, the coloredmesh to modify. It must have data values in
  `metadata$src_data` which can be mapped to the vertices of the mesh.

- hemi:

  character string, one of 'lh' or 'rh', the hemisphere of the mesh.
  Used to look up the data values in the hemilist `metadata$src_data`.

## Value

fs.coloredmesh, the modified coloredmesh. If the mesh contains no NaN
data (or the data cannot be mapped to the vertices), the input is
returned unchanged. If all vertices are NaN, the returned mesh has the
property 'render' set to FALSE, which makes the rendering functions skip
it.
