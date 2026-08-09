# Create a coloredmesh from an annotation of an atlas.

Create a coloredmesh from an annotation of an atlas.

## Usage

``` r
coloredmesh.from.annot(
  subjects_dir,
  subject_id,
  atlas,
  hemi,
  surface = "white",
  outline = FALSE
)
```

## Arguments

- subjects_dir:

  string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the
  data for all your subjects, each in a subdir named after the subject
  identifier.

- subject_id:

  string. The subject identifier.

- atlas:

  string or a loaded annotation. If a string, interpreted as the atlas
  name that should be loaded to get te annotation. E.g., "aparc",
  "aparc.2009s", or "aparc.DKTatlas". Used to construct the name of the
  annotation file to be loaded.

- hemi:

  string, one of 'lh' or 'rh'. The hemisphere name. Used to construct
  the names of the label data files to be loaded.

- surface:

  character string or `fs.surface` instance. The display surface. E.g.,
  "white", "pial", or "inflated". Defaults to "white".

- outline:

  logical, whether to draw an outline only instead of filling the
  regions. Defaults to FALSE. Only makes sense if you did not pass an
  outline already. The current implementation for outline computation is
  rather slow, so setting this to TRUE will considerably increase
  computation time.

## Value

coloredmesh. A named list with entries: "mesh" the
[`tmesh3d`](https://dmurdoch.github.io/rgl/dev/reference/mesh3d.html)
mesh object. "col": the mesh colors. "render", logical, whether to
render the mesh. "hemi": the hemisphere, one of 'lh' or 'rh'.

## See also

Other coloredmesh functions:
[`coloredmesh.from.label()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.label.md),
[`coloredmesh.from.mask()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.mask.md),
[`coloredmesh.from.morph.native()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.morph.native.md),
[`coloredmesh.from.morph.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.morph.standard.md),
[`coloredmesh.from.morphdata()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.morphdata.md),
[`coloredmeshes.from.color()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmeshes.from.color.md)
