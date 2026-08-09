# Construct filepath of any freesurfer file.

Construct filepath of any freesurfer file.

## Usage

``` r
subject.filepath.any(
  subjects_dir,
  subject_id,
  relative_path_parts,
  hemi = NULL,
  file_tag = "",
  warn_if_nonexistent = FALSE
)
```

## Arguments

- subjects_dir:

  character string. The FreeSurfer SUBJECTS_DIR, i.e., a directory
  containing the data for all your subjects, each in a subdir named
  after the subject identifier.

- subject_id:

  character string. The subject identifier. Can be a vector of subject
  identifiers.

- relative_path_parts:

  vector of strings. The path to the file, e.g., c("surf", "lh.area").

- hemi:

  string, one of 'lh', 'rh', or NULL. Defaults to NULL. If a hemisphere
  name is given, it is added as a prefix to the last entry in
  relative_path_parts, separated by a dot.

- file_tag:

  string. A one-word description of the file type that will show up in
  the error message to describe the file if it is missing. Leads to a
  better error message. Examples: 'morphometry' or 'label'. Only
  relevant if warn_if_nonexistent is TRUE. Defaults to the empty string.

- warn_if_nonexistent, :

  logical. Whether to print a warning if the file does not exist or
  cannot be accessed. Defaults to FALSE.

## Value

string, the file path. (Or a vector of strings if 'subject_id' is a
vector).

## Examples

``` r
if (FALSE) { # \dontrun{
fsbrain:::subject.filepath.any("/data/study1", "subject1",
  c("surf", "area"), hemi="lh");
fsbrain:::subject.filepath.any("/data/study1", c("subject1", "subject2"),
  c("surf", "area"), hemi="lh");
} # }
```
