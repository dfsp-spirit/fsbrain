# Retrieve native space morphometry data for a single subject.

Load native space morphometry data (like 'surf/lh.area') for a subject
from disk. Uses knowledge about the FreeSurfer directory structure to
load the correct file.

## Usage

``` r
subject.morph.native(
  subjects_dir,
  subject_id,
  measure,
  hemi,
  format = "curv",
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

  string, one of 'lh', 'rh' or 'both'. The hemisphere name. Used to
  construct the names of the annotation and morphometry data files to be
  loaded.

- format:

  string. One of 'mgh', 'mgz', 'curv'. Defaults to 'curv'.

- cortex_only:

  logical, whether to mask the medial wall, i.e., whether the
  morphometry data for all vertices which are *not* part of the cortex
  (as defined by the label file `label/?h.cortex.label`) should be
  replaced with NA values. In other words, setting this to TRUE will
  ignore the values of the medial wall between the two hemispheres. If
  set to true, the mentioned label file needs to exist for the subject.
  Defaults to FALSE.

- split_by_hemi:

  logical, whether the returned data should be encapsulated in a named
  list, where the names are from 'lh' and 'rh', and the values are the
  respective data.

## Value

vector with native space morph data, as returned by
[`read.fs.morph`](https://rdrr.io/pkg/freesurferformats/man/read.fs.morph.html).

## See also

Other morphometry data functions:
[`apply.label.to.morphdata()`](https://dfsp-spirit.github.io/fsbrain/reference/apply.label.to.morphdata.md),
[`apply.labeldata.to.morphdata()`](https://dfsp-spirit.github.io/fsbrain/reference/apply.labeldata.to.morphdata.md),
[`group.morph.native()`](https://dfsp-spirit.github.io/fsbrain/reference/group.morph.native.md),
[`group.morph.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/group.morph.standard.md),
[`subject.morph.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.morph.standard.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_optional_data();
   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");

   # Load the full data:
   thickness_lh = subject.morph.native(subjects_dir, "subject1", "thickness", "lh");
   mean(thickness_lh);  # prints 2.437466

   # Load the data again, but this time exclude the medial wall:
   thickness_lh_cortex = subject.morph.native(subjects_dir, "subject1", "thickness",
    "lh", cortex_only=TRUE);
   mean(thickness_lh_cortex, na.rm=TRUE);     # prints 2.544132
   vis.data.on.subject(subjects_dir, "subject1", thickness_lh_cortex, NULL);
} # }
```
