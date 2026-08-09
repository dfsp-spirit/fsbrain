# Construct filepath of native space morphometry data file.

Construct filepath of native space morphometry data file.

## Usage

``` r
subject.filepath.morph.native(
  subjects_dir,
  subject_id,
  measure,
  hemi,
  format = "curv",
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

  string. The subject identifier

- measure:

  string. Name of the vertex-wise measure of morphometry data file.
  E.g., "area" or "thickness". Used to construct the name of the
  morphometry file to be loaded.

- hemi:

  string, one of 'lh' or 'rh'. The hemisphere name.

- format, :

  string. One of 'mgh', 'mgz', 'curv'. Defaults to 'curv'.

- warn_if_nonexistent:

  logical. Whether to print a warning if the file does not exist or
  cannot be accessed. Defaults to FALSE.

- error_if_nonexistent:

  logical. Whether to raise an error if the file does not exist or
  cannot be accessed. Defaults to FALSE.

## Value

string, the file path.
