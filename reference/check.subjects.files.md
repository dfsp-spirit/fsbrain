# Report subjects missing files

Report subjects missing files

## Usage

``` r
check.subjects.files(
  subjects_dir,
  subjects_list,
  sfiles = c("surf/lh.thickness", "surf/rh.thickness", "surf/lh.white", "surf/rh.white",
    "label/lh.aparc.annot", "label/rh.aparc.annot")
)
```

## Arguments

- subjects_dir:

  string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the
  data for all your subjects, each in a subdir named after the subject
  identifier.

- subjects_list:

  vector of strings. The subject identifiers.

- sfiles:

  vector of character strings, the file names, relative to the data dir
  of the subject.

## Value

vector of character strings, the subjects missing any of the files.
