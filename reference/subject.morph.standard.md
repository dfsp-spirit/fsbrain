# Retrieve standard space morphometry data for a single subject.

Load standard space morphometry data (like
'surf/lh.area.fwhm10.fsaverage.mgh') for a subject from disk. Uses
knowledge about the FreeSurfer directory structure to load the correct
file.

## Usage

``` r
subject.morph.standard(
  subjects_dir,
  subject_id,
  measure,
  hemi,
  fwhm = "10",
  template_subject = "fsaverage",
  format = "mgh",
  cortex_only = FALSE,
  split_by_hemi = FALSE
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

  string, one of 'lh', 'rh', or 'both'. The hemisphere name. Used to
  construct the names of the annotation and morphometry data files to be
  loaded.

- fwhm:

  string. Smoothing as string, e.g. '10' or '25'.

- template_subject:

  string. Template subject name, defaults to 'fsaverage'.

- format:

  string. One of 'mgh', 'mgz', 'curv'. Defaults to 'mgh'.

- cortex_only:

  logical, whether to mask the medial wall, i.e., whether the
  morphometry data for all vertices which are *not* part of the cortex
  (as defined by the label file `label/?h.cortex.label`) should be
  replaced with NA values. In other words, setting this to TRUE will
  ignore the values of the medial wall between the two hemispheres. If
  set to true, the mentioned label file needs to exist for the template
  subject. Defaults to FALSE.

- split_by_hemi:

  logical, whether the returned data should be encapsulated in a named
  list, where the names are from 'lh' and 'rh', and the values are the
  respective data.

## Value

vector with standard space morph data

## See also

Other morphometry data functions:
[`apply.label.to.morphdata()`](https://dfsp-spirit.github.io/fsbrain/reference/apply.label.to.morphdata.md),
[`apply.labeldata.to.morphdata()`](https://dfsp-spirit.github.io/fsbrain/reference/apply.labeldata.to.morphdata.md),
[`group.morph.native()`](https://dfsp-spirit.github.io/fsbrain/reference/group.morph.native.md),
[`group.morph.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/group.morph.standard.md),
[`subject.morph.native()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.morph.native.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_optional_data();
   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
   thickness_lh = subject.morph.standard(subjects_dir, "subject1", "thickness", "lh", fwhm='10');
} # }
```
