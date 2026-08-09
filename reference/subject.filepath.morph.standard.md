# Construct filepath of standard space morphometry data file.

Construct filepath of standard space morphometry data file.

## Usage

``` r
subject.filepath.morph.standard(
  subjects_dir,
  subject_id,
  measure,
  hemi,
  fwhm = "10",
  template_subject = "fsaverage",
  format = "auto",
  warn_if_nonexistent = FALSE,
  error_if_nonexistent = FALSE
)
```

## Arguments

- subjects_dir:

  string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the
  data for all your subjects, each in a subdir named after the subject
  identifier.

- subject_id:

  string. The subject identifier. Can be a vector.

- measure:

  string. Name of the vertex-wise measure of morphometry data file.
  E.g., "area" or "thickness". Used to construct the name of the
  morphometry file to be loaded.

- hemi:

  string, one of 'lh' or 'rh'. The hemisphere name.

- fwhm:

  string. Smoothing as string, e.g. '10' or '25'. Defaults to '10'.

- template_subject:

  string. Template subject name, defaults to 'fsaverage'.

- format:

  string. One of 'mgh', 'mgz', 'curv'. Defaults to 'mgh'.

- warn_if_nonexistent:

  logical. Whether to print a warning if the file does not exist or
  cannot be accessed. Defaults to FALSE.

- error_if_nonexistent:

  logical. Whether to raise an error if the file does not exist or
  cannot be accessed. Defaults to FALSE.

## Value

string, the file path. (Or a vector if 'subject_id' is a vector.)
