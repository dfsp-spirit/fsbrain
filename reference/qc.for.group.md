# Perform data quality check based on computed region stats.

Determine subjects that potentially failed segmentation, based on
region-wise morphometry data. The stats can be computed from any kind of
data, but something like area or volume most likely works best. The
stats are based on the mean of the region values, so the measure should
at least roughly follow a normal distribution.

## Usage

``` r
qc.for.group(subjects_dir, subjects_list, measure, atlas, hemi = "both", ...)
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

- atlas:

  string. The atlas name. E.g., "aparc", "aparc.2009s", or
  "aparc.DKTatlas". Used to construct the name of the annotation file to
  be loaded.

- hemi:

  string, one of 'lh', 'rh', 'split', or 'both'. The hemisphere name.
  Used to construct the names of the annotation and morphometry data
  files to be loaded. If set to 'both', combined data for 'lh' and 'rh'
  will be used. If 'split', the data for hte two hemispheres will go
  into seprate columns, with column names having 'lh\_' and 'rh\_'
  prefixes.

- ...:

  parameters passed to
  [`qc.from.regionwise.df`](https://dfsp-spirit.github.io/fsbrain/reference/qc.from.regionwise.df.md).

## Value

qc result as a hemilist, each entry contains a named list as returned by
[`qc.from.regionwise.df`](https://dfsp-spirit.github.io/fsbrain/reference/qc.from.regionwise.df.md).

## See also

Other quality check functions:
[`qc.from.regionwise.df()`](https://dfsp-spirit.github.io/fsbrain/reference/qc.from.regionwise.df.md),
[`qc.from.segstats.table()`](https://dfsp-spirit.github.io/fsbrain/reference/qc.from.segstats.table.md)
