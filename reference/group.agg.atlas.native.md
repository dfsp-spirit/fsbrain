# Aggregate native space morphometry data over brain atlas regions and subjects for a group of subjects.

Aggregate native space morphometry data over brain atlas regions, e.g.,
compute the mean thickness value over all regions in an atlas for all
subjects.

## Usage

``` r
group.agg.atlas.native(
  subjects_dir,
  subjects_list,
  measure,
  hemi,
  atlas,
  agg_fun = mean,
  cache_file = NULL
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

- measure, :

  string. Name of the vertex-wise measure of morphometry data file.
  E.g., "area" or "thickness". Used to construct the name of the
  morphometry file to be loaded.

- hemi, :

  string, one of 'lh', 'rh', 'split', or 'both'. The hemisphere name.
  Used to construct the names of the annotation and morphometry data
  files to be loaded. If set to 'both', combined data for 'lh' and 'rh'
  will be used. If 'split', the data for hte two hemispheres will go
  into seprate columns, with column names having 'lh\_' and 'rh\_'
  prefixes.

- atlas, :

  string. The atlas name. E.g., "aparc", "aparc.2009s", or
  "aparc.DKTatlas". Used to construct the name of the annotation file to
  be loaded.

- agg_fun, :

  function. An R function that aggregates data, typically max, mean, min
  or something similar. Note: this is NOT a string, put the function
  name without quotes. Defaults to mean.

- cache_file, :

  string or NULL. If given, it is interpreted as path of a file, and the
  data will be cached in the file cache_file in RData format. If the
  file does not exist yet, the function will run and cache the data in
  the file. If the file exists, the function will load the data from the
  file instead of running. The filename should end in '.RData', but that
  is not enforced or checked in any way. WARNING: If cached data is
  returned, all parameters passed to this function (with the exception
  of 'cache_file') are ignored! Whether the cached data is for another
  subjects_list or hemi is NOT checked! You have to ensure this
  yourself, by using different filenames. Defaults to NULL.

## Value

dataframe with aggregated values for all regions and subjects, with n
columns and m rows, where n is the number of subjects and m is the
number of regions.

## See also

Other aggregation functions:
[`group.agg.atlas.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/group.agg.atlas.standard.md),
[`group.morph.agg.standard.vertex()`](https://dfsp-spirit.github.io/fsbrain/reference/group.morph.agg.standard.vertex.md),
[`subject.atlas.agg()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.atlas.agg.md)

Other atlas functions:
[`get.atlas.region.names()`](https://dfsp-spirit.github.io/fsbrain/reference/get.atlas.region.names.md),
[`group.agg.atlas.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/group.agg.atlas.standard.md),
[`group.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/group.annot.md),
[`group.label.from.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/group.label.from.annot.md),
[`label.from.annotdata()`](https://dfsp-spirit.github.io/fsbrain/reference/label.from.annotdata.md),
[`label.to.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/label.to.annot.md),
[`regions.to.ignore()`](https://dfsp-spirit.github.io/fsbrain/reference/regions.to.ignore.md),
[`spread.values.over.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/spread.values.over.annot.md),
[`spread.values.over.hemi()`](https://dfsp-spirit.github.io/fsbrain/reference/spread.values.over.hemi.md),
[`spread.values.over.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/spread.values.over.subject.md),
[`subject.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.annot.md),
[`subject.atlas.agg()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.atlas.agg.md),
[`subject.label.from.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.label.from.annot.md),
[`subject.lobes()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.lobes.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_optional_data();
   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
   agg = group.agg.atlas.native(subjects_dir, c('subject1', 'subject2'),
    'thickness', 'lh', 'aparc');
   # Visualize the mean values. Could use any subject, typically
   # one would use fsaverage. Here we use subject1:
   agg$subject = NULL;   # remove non-numeric column.
   vis.region.values.on.subject(subjects_dir, 'subject1', 'aparc',
    lh_region_value_list=colMeans(agg), rh_region_value_list=NULL);
} # }
```
