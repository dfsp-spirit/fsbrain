# Aggregate standard space morphometry data over subjects.

Aggregate vertex-wise values over subjects, leading to one aggregated
measure per vertex.

## Usage

``` r
group.morph.agg.standard.vertex(
  subjects_dir,
  subjects_list,
  measure,
  hemi,
  fwhm,
  agg_fun = mean,
  template_subject = "fsaverage",
  format = "mgh",
  cortex_only = FALSE,
  agg_fun_extra_params = NULL,
  split_by_hemi = FALSE
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

- measure:

  string. Name of the vertex-wise measure of morphometry data file.
  E.g., "area" or "thickness". Used to construct the name of the
  morphometry file to be loaded.

- hemi:

  string, one of 'lh', 'rh' or 'both'. The hemisphere name. Used to
  construct the names of the annotation and morphometry data files to be
  loaded.

- fwhm:

  string. Smoothing as string, e.g. '10' or '25'.

- agg_fun:

  function. An R function that aggregates data, typically
  [`max`](https://rdrr.io/r/base/Extremes.html), mean, min or something
  similar. Note: this is NOT a string, put the function name without
  quotes. Defaults to mean.

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

- split_by_hemi:

  logical, whether to return a hemilist

## See also

Other aggregation functions:
[`group.agg.atlas.native()`](https://dfsp-spirit.github.io/fsbrain/reference/group.agg.atlas.native.md),
[`group.agg.atlas.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/group.agg.atlas.standard.md),
[`subject.atlas.agg()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.atlas.agg.md)
