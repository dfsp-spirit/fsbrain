# Write standard space group data to a standard FreeSurfer directory stucture.

Write standard space group data to a standard FreeSurfer directory
stucture.

## Usage

``` r
write.group.morph.standard(
  subjects_dir,
  subjects_list,
  data,
  measure_name,
  hemi = "both",
  fwhm = "10",
  template_subject = "fsaverage",
  format = "mgh",
  create_dirs = TRUE,
  template_lh_numverts = NULL
)
```

## Arguments

- subjects_dir:

  string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the
  data for all your subjects, each in a subdir named after the subject
  identifier.

- subjects_list:

  vector of strings. The subject identifiers.

- data:

  the data matrix

- measure_name:

  character string, the data part of the generated file names, e.g.,
  'thickness' or 'area'.

- hemi:

  string, one of 'lh', 'rh' or 'both'. The hemisphere name. Used to
  construct the names of the annotation and morphometry data files to be
  loaded.

- fwhm:

  string. Smoothing as string, e.g. '10' or '25'.

- template_subject:

  string. Template subject name, defaults to 'fsaverage'.

- format:

  string. One of 'mgh', 'mgz', 'curv'. Defaults to 'mgh'.

- create_dirs:

  logical, whether to create missing (sub) directories which occur in
  the 'filepaths'.

- template_lh_numverts:

  positive integer, the vertex count of the left hemi of the template
  subject, only used if 'hemi' is 'both'. If hemi is both and this is
  unspecified (left at the default value `NULL`), the template subject
  needs to exist in the 'subjects_dir' to determine the vertex count of
  the left hemisphere, so that the data can be split into the `lh` and
  `rh` files at the correct index.

## See also

[`write.group.morph.standard.sf`](https://dfsp-spirit.github.io/fsbrain/reference/write.group.morph.standard.sf.md)
and
[`write.group.morph.standard.mf`](https://dfsp-spirit.github.io/fsbrain/reference/write.group.morph.standard.mf.md)

## Examples

``` r
if (FALSE) { # \dontrun{
dm = matrix(rnorm(325684 * 6, 5.0, 1.2), ncol = 6);
subjects = paste("subject", seq(6), sep="");
write.group.morph.standard("/tmp/groupdata", subjects, dm,
  "rand", template_lh_numverts = 325684 / 2);
} # }
```
