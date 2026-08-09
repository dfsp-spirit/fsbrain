# Create visual quality check report from QC result.

Create visual quality check report from QC result.

## Usage

``` r
subject.report.html(
  subjects_dir,
  subjects_list,
  subjects_metadata = list(),
  out_dir = "fsbrain_qc_report",
  keep_existing_images = TRUE
)
```

## Arguments

- subjects_dir:

  string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the
  data for all your subjects, each in a subdir named after the subject
  identifier.

- subjects_list:

  string vector. A vector of subject identifiers that match the
  directory names within subjects_dir.

- subjects_metadata:

  named list, keys can be subjects from subjects_list. Each key can hold
  another named list of strings, represeting arbitrary metadata for that
  subject that will be displayed in the report.

- out_dir:

  character string, path to output dir. The last directory part will be
  created if it does not exist (but not recursively).

- keep_existing_images:

  logical, whether to keep existing images. A lot faster on 2nd call.
