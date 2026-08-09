# Visualize the number of outlier subjects per region in your dataset.

The function helps you to see which regions are affected the most by QC
issues: for each region, it plots the number of subjects which are
outliers in the region.

## Usage

``` r
qc.vis.failcount.by.region(
  qc_res,
  atlas,
  subjects_dir = fsaverage.path(),
  subject_id = "fsaverage",
  ...
)
```

## Arguments

- qc_res:

  hemilist of QC results, as returned by functions like
  [`qc.for.group`](https://dfsp-spirit.github.io/fsbrain/reference/qc.for.group.md)
  or
  [`qc.from.segstats.tables`](https://dfsp-spirit.github.io/fsbrain/reference/qc.from.segstats.tables.md).

- atlas:

  string. The brain atlas to use. E.g., 'aparc' or 'aparc.a2009s'.

- subjects_dir:

  string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the
  data for all your subjects, each in a subdir named after the subject
  identifier.

- subject_id:

  string. The subject identifier.

- ...:

  extra parameters passed to
  [`vis.region.values.on.subject`](https://dfsp-spirit.github.io/fsbrain/reference/vis.region.values.on.subject.md).
  E.g., to change to interactive view, get a colorbar and better
  resolution, try: `draw_colorbar=T, rgloptions = rglo(), views='si'`.

## Note

You can visualize this on any subject you like, 'fsaverage' is a typical
choice. The atlas must be the one used during the QC step.
