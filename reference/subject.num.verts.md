# Get subjects vertex count.

Determine vertex counts for the brain meshes of a subject.

## Usage

``` r
subject.num.verts(
  subjects_dir,
  subject_id,
  surface = "white",
  hemi = "both",
  do_sum = FALSE
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

- do_sum:

  logical, whether to return the sum of the vertex counts for lh and rh.
  Ignored unless 'hemi' is 'both'. If set, a single scalar will be
  returned.

## Value

hemilist of integers, the vertex count. If hemi is 'both' and 'do_sum'
is `FALSE`, a hemilist of integers is returned. Otherwise, a single
integer.
