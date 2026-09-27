# Create a coloredmesh from a mesh and pre-defined colors.

Create a coloredmesh from a mesh and pre-defined colors.

## Usage

``` r
coloredmesh.from.color(
  subjects_dir,
  subject_id,
  color_data,
  hemi,
  surface = "white",
  metadata = list(),
  style = NULL
)
```

## Arguments

- subjects_dir:

  string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the
  data for all your subjects, each in a subdir named after the subject
  identifier.

- subject_id:

  string. The subject identifier.

- color_data:

  vector of hex color strings, a single one or one per vertex.

- hemi:

  string, one of 'lh' or 'rh'. The hemisphere name. Used to construct
  the names of the label data files to be loaded.

- surface:

  character string or `fs.surface` instance. The display surface. E.g.,
  "white", "pial", or "inflated". Defaults to "white".

- metadata:

  a named list, can contain whatever you want. Typical entries are:
  'src_data' a hemilist containing the source data from which the
  'color_data' was created, optional. If available, it is encoded into
  the coloredmesh and can be used later to plot a colorbar.
  'makecmap_options': the options used to created the colormap from the
  data.

- style:

  `NULL` or a rendering style for this mesh, see
  [`get.rglstyle`](https://dfsp-spirit.github.io/fsbrain/reference/get.rglstyle.md).
  Styles can be a style name (like 'default' or 'glass') or a named list
  of material properties (like `list('alpha'=0.2)`). The style is stored
  in the 'style' field of the returned coloredmesh and is used when the
  mesh is rendered with `style='from_mesh'`, see
  [`vis.coloredmeshes`](https://dfsp-spirit.github.io/fsbrain/reference/vis.coloredmeshes.md).
  This is how you can give individual meshes in a scene their own look,
  e.g., to draw a cortex mesh semi-transparently behind colored data
  meshes.

## Value

coloredmesh. A named list with entries: "mesh" the
[`tmesh3d`](https://dmurdoch.github.io/rgl/dev/reference/mesh3d.html)
mesh object. "col": the mesh colors. "render", logical, whether to
render the mesh. "hemi": the hemisphere, one of 'lh' or 'rh'. If not
`NULL`, also "style": the rendering style for this mesh.

## Note

You will usually not call this directly, but use
[`coloredmeshes.from.color`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmeshes.from.color.md)
or
[`vis.color.on.subject`](https://dfsp-spirit.github.io/fsbrain/reference/vis.color.on.subject.md)
instead. It is exported for cases in which you want to build a single
hemisphere of a scene manually.

## See also

Other coloredmesh functions:
[`Triangles3D.to.coloredmesh()`](https://dfsp-spirit.github.io/fsbrain/reference/Triangles3D.to.coloredmesh.md),
[`coloredmesh.from.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.annot.md),
[`coloredmesh.from.label()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.label.md),
[`coloredmesh.from.mask()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.mask.md),
[`coloredmesh.from.morph.native()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.morph.native.md),
[`coloredmesh.from.morph.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.morph.standard.md),
[`coloredmesh.from.morphdata()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.morphdata.md),
[`coloredmeshes.from.color()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmeshes.from.color.md)
