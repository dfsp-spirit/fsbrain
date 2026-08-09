# Aggregate native space morphometry data over one hemisphere for a group of subjects.

Compute the mean (or other aggregates) over all vertices of a subject
from native space morphometry data (like 'surf/lh.area'). Uses knowledge
about the FreeSurfer directory structure to load the correct file.

## Usage

``` r
group.morph.agg.native(
  subjects_dir,
  subjects_list,
  measure,
  hemi,
  agg_fun = mean,
  cast = TRUE,
  format = "curv",
  cortex_only = FALSE,
  agg_fun_extra_params = NULL
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

- measure, :

  string. Name of the vertex-wise measure of morphometry data file.
  E.g., "area" or "thickness". Used to construct the name of the
  morphometry file to be loaded.

- hemi, :

  string, one of 'lh', 'rh' or 'both'. The hemisphere name. Used to
  construct the names of the annotation and morphometry data files to be
  loaded.

- agg_fun:

  function. An R function that aggregates data, typically
  [`max`](https://rdrr.io/r/base/Extremes.html), mean, min or something
  similar. Note: this is NOT a string, put the function name without
  quotes. Defaults to mean.

- cast:

  Whether a separate 'hemi' column should exist.

- format:

  string. One of 'mgh', 'mgz', 'curv'. Defaults to 'mgh'.

- cortex_only:

  logical, whether to mask the medial wall, i.e., whether the
  morphometry data for all vertices which are *not* part of the cortex
  (as defined by the label file `label/?h.cortex.label`) should be
  replaced with NA values. In other words, setting this to TRUE will
  ignore the values of the medial wall between the two hemispheres. If
  set to true, the mentioned label file needs to exist for the subjects.
  Also not that the aggregation function will need to be able to cope
  with NA values if you set this to TRUE. You can use
  'agg_fun_extra_params' if needed to achieve that, depending on the
  function. Foe example, if you use the
  [`mean`](https://rdrr.io/r/base/mean.html) function, you could set
  `agg_fun_extra_params=list("na.rm"=TRUE)` to get the mean of the
  vertices which are not masked. Defaults to FALSE.

- agg_fun_extra_params:

  named list, extra parameters to pass to the aggregation function
  'agg_fun' besides the loaded morphometry data. This is useful if you
  have masked the data and need to ignore NA values in the agg_fun.

## Value

dataframe with aggregated values for all subjects, with 3 columns and n
rows, where n is the number of subjects. The 3 columns are 'subject_id',
'hemi', and `'<measure>'` (e.g., "thickness"), the latter contains the
aggregated data.

## See also

Other global aggregation functions:
[`group.morph.agg.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/group.morph.agg.standard.md),
[`group.multimorph.agg.native()`](https://dfsp-spirit.github.io/fsbrain/reference/group.multimorph.agg.native.md),
[`group.multimorph.agg.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/group.multimorph.agg.standard.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_optional_data();
   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
   subjects_list = c("subject1", "subject2");
   fulldata = group.morph.agg.native(subjects_dir, subjects_list, "thickness", "lh");
   head(fulldata);
} # }
```
