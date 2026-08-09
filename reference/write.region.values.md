# Write one value per atlas region for a subject.

Given an atlas and a list that contains one value for each atlas region,
write a morphometry file in which all region vertices are assigned the
value. Can be used to plot stuff like p-values or effect sizes onto
brain regions.

## Usage

``` r
write.region.values(
  subjects_dir,
  subject_id,
  hemi,
  atlas,
  region_value_list,
  outfile_morph_name,
  format = "mgz",
  do_write_file = TRUE,
  output_path = NULL,
  value_for_unlisted_regions = NaN
)
```

## Arguments

- subjects_dir, :

  string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the
  data for all your subjects, each in a subdir named after the subject
  identifier.

- subject_id, :

  string. The subject identifier

- hemi, :

  string, one of 'lh' or 'rh'. The hemisphere name. Used to construct
  the names of the annotation and morphometry data files to be loaded.

- atlas, :

  string. The atlas name. E.g., "aparc", "aparc.2009s", or
  "aparc.DKTatlas". Used to construct the name of the annotation file to
  be loaded.

- region_value_list, :

  named list. A list in which the names are atlas regions, and the
  values are the value to write to all vertices of that region.

- outfile_morph_name, :

  string. The measure part of the output file name. E.g.,
  'agg_thickness' will write the file
  `'<subject>/surf/<hemi>.agg_thickness.mgh'`.

- format, :

  string. A morphometry file format. One of 'mgh', 'mgz' or 'curv.' The
  output file name extension will be set accordingly. Defaults to 'mgz'.

- do_write_file, :

  logical. Whether to write the data to a file on the disk. If FALSE,
  the data are only returned (and the outfile_morph_name parameter gets
  ignored). Default to TRUE.

- output_path:

  string, path to the output directory. If omitted, defaults to the
  'surf' directory of the subject (i.e.,
  `'<subjects_dir>/<subject_id>/surf/'`).

- value_for_unlisted_regions, :

  numeric scalar. The value to assign to vertices which are part of
  atlas regions that are not listed in region_value_list. Defaults to
  NaN.

## Value

a named list with the following entries: "data": a vector containing the
data. "file_written": string, path to the file that was written, only
exists if do_write = TRUE.

## See also

Other output functions:
[`write.region.aggregated()`](https://dfsp-spirit.github.io/fsbrain/reference/write.region.aggregated.md),
[`write.region.values.fsaverage()`](https://dfsp-spirit.github.io/fsbrain/reference/write.region.values.fsaverage.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_optional_data();
   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
   region_value_list = list("bankssts"=0.9, "precuneus"=0.7);
   write.region.values(subjects_dir, 'subject1', 'lh', 'aparc',
    region_value_list, 'pvalues.mgz', do_write_file = FALSE);
} # }
```
