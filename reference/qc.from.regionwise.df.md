# Perform data quality check based on a dataframe containing aggregated region-wise data.

Determine subjects that potentially failed segmentation, based on
region-wise data. The data can be anything, but there must be one
numerical value per subject per region.

## Usage

``` r
qc.from.regionwise.df(
  rdf,
  z_threshold = 2.8,
  verbosity = 0L,
  num_bad_regions_allowed = 1L
)
```

## Arguments

- rdf:

  data.frame, the region data. The first column must contain the subject
  identifier, all other columns should contain numerical data for a
  single region. (Each row represents a subject.) This can be produced
  by calling
  [`group.agg.atlas.native`](https://dfsp-spirit.github.io/fsbrain/reference/group.agg.atlas.native.md)
  or by parsing a text file produced by the FreeSurfer tool
  'aparcstats2table' (see `fsbrain:::qc.from.segstats.table` for parsing
  code).

- z_threshold:

  numerical, the cutoff value for considering a subject an outlier (in
  standard deviations).

- verbosity:

  integer, controls the output to stdout. 0=off, 1=normal, 2=verbose.

- num_bad_regions_allowed:

  integer, the number of regions in which subjects are allowed to be
  outliers without being reported as potentially failed segmentation

## Value

named list with entries: 'failed_subjects': vector of character strings,
the subject identifiers which potentially failed segmentation.
'mean_dists_z': distance to mean, in standard deviations, per subject
per region. 'num_outlier_subjects_per_region': number of outlier
subjects by region. 'metadata': named list of metadata, e.g., hemi,
atlas and measure used to compute these QC results.

## See also

Other quality check functions:
[`qc.for.group()`](https://dfsp-spirit.github.io/fsbrain/reference/qc.for.group.md),
[`qc.from.segstats.table()`](https://dfsp-spirit.github.io/fsbrain/reference/qc.from.segstats.table.md)
