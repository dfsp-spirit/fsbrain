# Aggregate morphometry data over brain atlas regions for a subject.

Aggregate morphometry data over brain atlas regions, e.g., compute the
mean thickness value over all regions in an atlas.

## Usage

``` r
subject.atlas.agg(
  vertex_morph_data,
  vertex_label_names,
  agg_fun = base::mean,
  requested_label_names = c()
)
```

## Arguments

- vertex_morph_data, :

  numeric vector. The morphometry data, one value per vertex. The
  morphometry data are typically loaded from an MGZ or curv format file
  with the read.fs.curv or read.fs.mgh functions.

- vertex_label_names, :

  string vector. The region names for the vertices, one string per
  vertex. The region names are typically loaded from an annotation file
  with the read.fs.annot function.

- agg_fun, :

  function. An R function that aggregates data, typically max, mean, min
  or something similar. Note: this is NOT a string, put the function
  name without quotes. Defaults to
  [`base::mean`](https://rdrr.io/r/base/mean.html).

- requested_label_names, :

  string vector. The label (or region) names that you want to occur in
  the output. If not specified, all region names which occur in the data
  are used. If given, and one of the requested names does NOT occur in
  the data, it will occur in the output with aggregation value NaN. If
  given, and one of the names from the data does NOT occur in the
  requested list, it will NOT occur in the output. So if you specify
  this, the output dataframe will contain a row for a region if and only
  if it is in the requested list.

## Value

dataframe with aggregated values for all regions, with 2 columns and n
rows, where n is the number of effective regions. The columns are:
"region": string, contains the region name. "aggregated": numeric,
contains the result of applying agg_fun to the morphometry data in that
region.

## See also

Other aggregation functions:
[`group.agg.atlas.native()`](https://dfsp-spirit.github.io/fsbrain/reference/group.agg.atlas.native.md),
[`group.agg.atlas.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/group.agg.atlas.standard.md),
[`group.morph.agg.standard.vertex()`](https://dfsp-spirit.github.io/fsbrain/reference/group.morph.agg.standard.vertex.md)

Other atlas functions:
[`get.atlas.region.names()`](https://dfsp-spirit.github.io/fsbrain/reference/get.atlas.region.names.md),
[`group.agg.atlas.native()`](https://dfsp-spirit.github.io/fsbrain/reference/group.agg.atlas.native.md),
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
[`subject.label.from.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.label.from.annot.md),
[`subject.lobes()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.lobes.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_optional_data();
   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
   morph_data = subject.morph.native(subjects_dir, 'subject1', 'thickness', 'lh');
   annot = subject.annot(subjects_dir, 'subject1', 'lh', 'aparc');
   agg = subject.atlas.agg(morph_data, annot$label_names);
} # }
```
