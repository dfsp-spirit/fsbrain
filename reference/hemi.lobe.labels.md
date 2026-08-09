# Compute lobe labels for a single hemi from aparc atlas.

Compute lobe labels for a single hemi from aparc atlas.

## Usage

``` r
hemi.lobe.labels(
  subjects_dir,
  subject_id,
  hemi,
  include_cingulate = TRUE,
  as_annot = FALSE
)
```

## Arguments

- subjects_dir:

  string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the
  data for all your subjects, each in a subdir named after the subject
  identifier.

- subject_id:

  string. The subject identifier

- hemi:

  string, one of 'lh' or 'rh'. The hemisphere name.

- include_cingulate:

  logical, whether to include the vertices of the cingulate in the lobes

- as_annot:

  return a hemilist of annotations instead of the return value described
  in the *value* section

## Note

See
[`subject.lobes`](https://dfsp-spirit.github.io/fsbrain/reference/subject.lobes.md)
for details.
