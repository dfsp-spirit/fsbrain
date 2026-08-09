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
  metadata = list()
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

## Value

coloredmesh. A named list with entries: "mesh" the
[`tmesh3d`](https://dmurdoch.github.io/rgl/dev/reference/mesh3d.html)
mesh object. "col": the mesh colors. "render", logical, whether to
render the mesh. "hemi": the hemisphere, one of 'lh' or 'rh'.

## Note

Do not call this directly, use
[`coloredmeshes.from.color`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmeshes.from.color.md)
instead.
