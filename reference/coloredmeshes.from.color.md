# Create coloredmeshes for both hemis using pre-defined colors.

Create coloredmeshes for both hemis using pre-defined colors.

## Usage

``` r
coloredmeshes.from.color(
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

  a hemilist containing vectors of hex color strings

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

named list of coloredmeshes. Each entry is a named list with entries:
"mesh" the
[`tmesh3d`](https://dmurdoch.github.io/rgl/dev/reference/mesh3d.html)
mesh object. "col": the mesh colors. "render", logical, whether to
render the mesh. "hemi": the hemisphere, one of 'lh' or 'rh'.

## See also

Other coloredmesh functions:
[`coloredmesh.from.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.annot.md),
[`coloredmesh.from.label()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.label.md),
[`coloredmesh.from.mask()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.mask.md),
[`coloredmesh.from.morph.native()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.morph.native.md),
[`coloredmesh.from.morph.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.morph.standard.md),
[`coloredmesh.from.morphdata()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.morphdata.md)
