# Create a coloredmesh from a mask.

Create a coloredmesh from a mask.

## Usage

``` r
coloredmesh.from.mask(
  subjects_dir,
  subject_id,
  mask,
  hemi,
  surface = "white",
  surface_data = NULL,
  makecmap_options = list(colFn = squash::rainbow2)
)
```

## Arguments

- subjects_dir:

  string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the
  data for all your subjects, each in a subdir named after the subject
  identifier.

- subject_id:

  string. The subject identifier.

- mask:

  logical vector, contains one logical value per vertex.

- hemi:

  string, one of 'lh' or 'rh'. The hemisphere name. Used to construct
  the names of the label data files to be loaded.

- surface:

  character string or `fs.surface` instance. The display surface. E.g.,
  "white", "pial", or "inflated". Defaults to "white".

- surface_data:

  optional surface mesh object, as returned by
  [`subject.surface`](https://dfsp-spirit.github.io/fsbrain/reference/subject.surface.md).
  If given, used instead of loading the surface data from disk (which
  users of this function may already have done). Defaults to NULL.

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

Other mask functions:
[`mask.from.labeldata.for.hemi()`](https://dfsp-spirit.github.io/fsbrain/reference/mask.from.labeldata.for.hemi.md),
[`vis.mask.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.mask.on.subject.md)

Other coloredmesh functions:
[`coloredmesh.from.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.annot.md),
[`coloredmesh.from.label()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.label.md),
[`coloredmesh.from.morph.native()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.morph.native.md),
[`coloredmesh.from.morph.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.morph.standard.md),
[`coloredmesh.from.morphdata()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.morphdata.md),
[`coloredmeshes.from.color()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmeshes.from.color.md)
