# Retrieve native space morphometry data for a group of subjects.

Load native space morphometry data (like 'surf/lh.area') for a group of
subjects from disk. Uses knowledge about the FreeSurfer directory
structure to load the correct file.

## Usage

``` r
group.morph.native(
  subjects_dir,
  subjects_list,
  measure,
  hemi,
  format = "curv",
  cortex_only = FALSE
)
```

## Arguments

- subjects_dir, :

  string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the
  data for all your subjects, each in a subdir named after the subject
  identifier.

- subjects_list, :

  vector of strings. The subject identifiers.

- measure, :

  string. Name of the vertex-wise measure of morphometry data file.
  E.g., "area" or "thickness". Used to construct the name of the
  morphometry file to be loaded.

- hemi, :

  string, one of 'lh', 'rh' or 'both'. The hemisphere name. Used to
  construct the names of the annotation and morphometry data files to be
  loaded.

- format, :

  string. One of 'mgh', 'mgz', 'curv'. Defaults to 'curv'.

- cortex_only:

  logical, whether to mask the medial wall, i.e., whether the
  morphometry data for all vertices which are *not* part of the cortex
  (as defined by the label file `label/?h.cortex.label`) should be
  replaced with NA values. In other words, setting this to TRUE will
  ignore the values of the medial wall between the two hemispheres. If
  set to true, the mentioned label file needs to exist for the subjects.
  Defaults to FALSE.

## Value

named list with native space morph data, the names are the subject
identifiers from the subjects_list, and the values are morphometry data
vectors (of different length, as each subject has a different vertex
count in native space).

## See also

Other morphometry data functions:
[`apply.label.to.morphdata()`](https://dfsp-spirit.github.io/fsbrain/reference/apply.label.to.morphdata.md),
[`apply.labeldata.to.morphdata()`](https://dfsp-spirit.github.io/fsbrain/reference/apply.labeldata.to.morphdata.md),
[`group.morph.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/group.morph.standard.md),
[`subject.morph.native()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.morph.native.md),
[`subject.morph.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.morph.standard.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_optional_data();
   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
   subjects_list = c("subject1", "subject2");
   data = group.morph.native(subjects_dir, subjects_list, "thickness", "lh");
} # }
```
