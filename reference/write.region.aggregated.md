# Write data aggregated over regions to morphometry file for group.

Given an atlas, a subjects list and a measure, aggregate the measure
over each region (e.g., mean) and write an output morphometry file in
which the value for all region vertices is set to the aggregated value.

## Usage

``` r
write.region.aggregated(
  subjects_dir,
  subjects_list,
  measure,
  hemi,
  atlas,
  agg_fun = mean,
  outfile_morph_name = "",
  format = "mgz"
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

  string, one of 'lh' or 'rh'. The hemisphere name. Used to construct
  the names of the annotation and morphometry data files to be loaded.

- atlas, :

  string. The atlas name. E.g., "aparc", "aparc.2009s", or
  "aparc.DKTatlas". Used to construct the name of the annotation file to
  be loaded.

- agg_fun, :

  function. An R function that aggregates data, typically max, mean, min
  or something similar. Note: this is NOT a string, put the function
  name without quotes. Defaults to mean.

- outfile_morph_name, :

  string. The measure part of the output file name. E.g.,
  'agg_thickness' will write the file
  `'<subject>/surf/<hemi>.agg_thickness.mgh'`. Defaults to
  `'agg_<measure>'`.

- format, :

  string. A morphometry file format. One of 'mgh', 'mgz' or 'curv.' The
  output file name extension will be set accordingly. Defaults to 'mgz'.

## See also

Other output functions:
[`write.region.values()`](https://dfsp-spirit.github.io/fsbrain/reference/write.region.values.md),
[`write.region.values.fsaverage()`](https://dfsp-spirit.github.io/fsbrain/reference/write.region.values.fsaverage.md)
