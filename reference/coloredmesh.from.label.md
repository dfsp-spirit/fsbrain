# Create a coloredmesh from a label.

Create a coloredmesh from a label.

## Usage

``` r
coloredmesh.from.label(
  subjects_dir,
  subject_id,
  label,
  hemi,
  surface = "white",
  makecmap_options = list(colFn = squash::rainbow2),
  binary = TRUE
)
```

## Arguments

- subjects_dir:

  string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the
  data for all your subjects, each in a subdir named after the subject
  identifier.

- subject_id:

  string. The subject identifier.

- label:

  string or vector of integers. If a string, the name of the label file,
  without the hemi part (if any), but including the '.label' suffix.
  E.g., 'cortex.label' for '?h.cortex.label'. Alternatively, the already
  loaded label data as a vector of integers.

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

- binary:

  logical, whether to treat the label as binary

## Value

coloredmesh. A named list with entries: "mesh" the
[`tmesh3d`](https://dmurdoch.github.io/rgl/dev/reference/mesh3d.html)
mesh object. "col": the mesh colors. "render", logical, whether to
render the mesh. "hemi": the hemisphere, one of 'lh' or 'rh'.

## See also

Other coloredmesh functions:
[`coloredmesh.from.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.annot.md),
[`coloredmesh.from.mask()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.mask.md),
[`coloredmesh.from.morph.native()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.morph.native.md),
[`coloredmesh.from.morph.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.morph.standard.md),
[`coloredmesh.from.morphdata()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.morphdata.md),
[`coloredmeshes.from.color()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmeshes.from.color.md)
