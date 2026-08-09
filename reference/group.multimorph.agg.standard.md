# Aggregate standard space (fsaverage) morphometry data for multiple measures over hemispheres for a group of subjects.

Compute the mean (or other aggregates) over all vertices of a subject
from standard space morphometry data (like
'surf/lh.area.fwhm10.fsaverage.mgh'). You can specify several measures
and hemispheres. Uses knowledge about the FreeSurfer directory structure
to load the correct files.

## Usage

``` r
group.multimorph.agg.standard(
  subjects_dir,
  subjects_list,
  measures,
  hemis,
  fwhm,
  agg_fun = mean,
  template_subject = "fsaverage",
  format = "mgh",
  cast = TRUE,
  cortex_only = FALSE,
  agg_fun_extra_params = NULL
)
```

## Arguments

- subjects_dir, :

  string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the
  data for all your subjects, each in a subdir named after the subject
  identifier.

- subjects_list, :

  string vector. A vector of subject identifiers that match the
  directory names within subjects_dir.

- measures, :

  vector of strings. Names of the vertex-wise morphometry measures.
  E.g., c("area", "thickness"). Used to construct the names of the
  morphometry file to be loaded.

- hemis, :

  string, one of 'lh', 'rh' or 'both'. The hemisphere name. Used to
  construct the names of the annotation and morphometry data files to be
  loaded.

- fwhm, :

  string. Smoothing as string, e.g. '10' or '25'.

- agg_fun, :

  function. An R function that aggregates data, typically
  [`max`](https://rdrr.io/r/base/Extremes.html), mean, min or something
  similar. Note: this is NOT a string, put the function name without
  quotes. Defaults to mean.

- template_subject, :

  string. Template subject name, defaults to 'fsaverage'.

- format, :

  string. One of 'mgh', 'mgz', 'curv'. Defaults to 'mgh'.

- cast, :

  Whether a separate 'hemi' column should exist.

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

dataframe with aggregated values over all measures and hemis for all
subjects, with m columns and n rows, where n is the number of subjects.
The m columns are 'subject_id' and `'<hemi>.<measure>'` (e.g.,
"lh.thickness") for all combinations of hemi and measure, the latter
contains the aggregated data.

## See also

Other global aggregation functions:
[`group.morph.agg.native()`](https://dfsp-spirit.github.io/fsbrain/reference/group.morph.agg.native.md),
[`group.morph.agg.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/group.morph.agg.standard.md),
[`group.multimorph.agg.native()`](https://dfsp-spirit.github.io/fsbrain/reference/group.multimorph.agg.native.md)
