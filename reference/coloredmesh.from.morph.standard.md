# Create a coloredmesh from standard space morphometry data.

Create a coloredmesh from standard space morphometry data.

## Usage

``` r
coloredmesh.from.morph.standard(
  subjects_dir,
  subject_id,
  measure,
  hemi,
  fwhm,
  surface = "white",
  template_subject = "fsaverage",
  template_subjects_dir = NULL,
  clip = NULL,
  cortex_only = FALSE,
  makecmap_options = mkco.seq()
)
```

## Arguments

- subjects_dir:

  string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the
  data for all your subjects, each in a subdir named after the subject
  identifier.

- subject_id:

  string. The subject identifier.

- measure:

  string. The morphometry data to use. E.g., 'area' or 'thickness'. Pass
  NULL to render the surface in white, without any data. One can also
  pass the pre-loaded morphometry data as a numerical vector, the length
  of which must match the number of surface vertices.

- hemi:

  string, one of 'lh' or 'rh'. The hemisphere name. Used to construct
  the names of the label data files to be loaded.

- fwhm:

  string, smoothing setting. The smoothing part of the filename,
  typically something like '0', '5', '10', ..., or '25'.

- surface:

  character string or `fs.surface` instance. The display surface. E.g.,
  "white", "pial", or "inflated". Defaults to "white".

- template_subject:

  The template subject used. This will be used as part of the filename,
  and its surfaces are loaded for data visualization. Defaults to
  'fsaverage'.

- template_subjects_dir:

  The template subjects dir. If `NULL`, the value of the parameter
  'subjects_dir' is used. Defaults to NULL. If you have FreeSurfer
  installed and configured, and are using the standard fsaverage
  subject, try passing the result of calling
  'file.path(Sys.getenv('FREESURFER_HOME'), 'subjects')'.

- clip:

  numeric vector of length 2 or NULL. If given, the 2 values are
  interpreted as lower and upper percentiles, and the morph data is
  clipped at the given lower and upper percentile (see
  [`clip.data`](https://dfsp-spirit.github.io/fsbrain/reference/clip.data.md)).
  Defaults to NULL (no data clipping).

- cortex_only:

  logical, whether to mask the medial wall, i.e., whether the
  morphometry data for all vertices which are *not* part of the cortex
  (as defined by the label file `label/?h.cortex.label`) should be
  replaced with NA values. In other words, setting this to TRUE will
  ignore the values of the medial wall between the two hemispheres. If
  set to true, the mentioned label file needs to exist for the subject.
  Defaults to FALSE.

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
[`coloredmesh.from.morphdata()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.morphdata.md),
[`coloredmeshes.from.color()`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmeshes.from.color.md)
