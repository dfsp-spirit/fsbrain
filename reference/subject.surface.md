# Load a surface for a subject.

Load a brain surface mesh for a subject.

## Usage

``` r
subject.surface(
  subjects_dir,
  subject_id,
  surface = "white",
  hemi = "both",
  force_hemilist = FALSE,
  as_tm = FALSE
)
```

## Arguments

- subjects_dir:

  string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the
  data for all your subjects, each in a subdir named after the subject
  identifier.

- subject_id:

  string. The subject identifier

- surface:

  string. The surface name. E.g., "white", or "pial". Used to construct
  the name of the surface file to be loaded.

- hemi:

  string, one of 'lh', 'rh', or 'both'. The hemisphere name. Used to
  construct the names of the surface file to be loaded. For 'both', see
  the information on the return value.

- force_hemilist:

  logical, whether to return a hemilist even if the 'hemi' parameter is
  not set to 'both'

- as_tm:

  logical, whether to return an
  [`rgl::tmesh3d`](https://dmurdoch.github.io/rgl/dev/reference/mesh3d.html)
  instead of an `fs.surface` instance by applying the
  `fs.surface.to.tmesh3d` function.

## Value

the `fs.surface` instance, as returned by
[`read.fs.surface`](https://rdrr.io/pkg/freesurferformats/man/read.fs.surface.html).
If parameter `hemi` is set to `both`, a named list with entries `lh` and
`rh` is returned, and the values of are the respective surfaces. The
mesh data structure used in `fs.surface` is a *face index set*.

## See also

Other surface mesh functions:
[`face.edges()`](https://dfsp-spirit.github.io/fsbrain/reference/face.edges.md),
[`label.border()`](https://dfsp-spirit.github.io/fsbrain/reference/label.border.md),
[`mesh.vertex.included.faces()`](https://dfsp-spirit.github.io/fsbrain/reference/mesh.vertex.included.faces.md),
[`mesh.vertex.neighbors()`](https://dfsp-spirit.github.io/fsbrain/reference/mesh.vertex.neighbors.md),
[`vis.path.along.verts()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.path.along.verts.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_optional_data();
   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
   lh_white = subject.surface(subjects_dir, "subject1", "white", "lh");
} # }
```
