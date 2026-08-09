# Create a coloredmesh from arbitrary data.

Create a coloredmesh from arbitrary data.

## Usage

``` r
coloredmesh.from.morphdata(
  subjects_dir,
  vis_subject_id,
  morph_data,
  hemi,
  surface = "white",
  makecmap_options = mkco.seq()
)
```

## Arguments

- subjects_dir:

  string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the
  data for all your subjects, each in a subdir named after the subject
  identifier.

- vis_subject_id:

  string. The subject identifier from which to obtain the surface for
  data visualization. Example: 'fsaverage'.

- morph_data:

  string. The morphometry data to use. E.g., 'area' or 'thickness.'

- hemi:

  string, one of 'lh' or 'rh'. The hemisphere name. Used to construct
  the names of the label data files to be loaded.

- surface:

  character string or `fs.surface` instance. The display surface. E.g.,
  "white", "pial", or "inflated". Defaults to "white".

- makecmap_options:

  named list of parameters to pass to
  [`makecmap`](https://rdrr.io/pkg/squash/man/makecmap.html). Must not
  include the unnamed first parameter, which is derived from 'measure'.

## Value

coloredmesh. A named list with entries: "mesh" the
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
[`coloredmeshes.from.color()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmeshes.from.color.md)
