# Concatenate native space data for a group of subjects.

A measure is something like 'thickness' or 'area'. This function
concatenates the native space data for all subjects into a single long
vector for each measure. A dataframe is then created, in which each
column is one such vector. This can be used to compute the correlation
between measures on vertex level, for example.

## Usage

``` r
group.concat.measures.native(
  subjects_dir,
  subjects_list,
  measures,
  hemi,
  cortex_only = FALSE
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

  vector of strings. Names of the vertex-wise morhometry measures. E.g.,
  c("area", "thickness"). Used to construct the names of the morphometry
  file to be loaded. The data of each measure will be one column in the
  resulting dataframe.

- hemi, :

  string, one of 'lh', 'rh' or 'both'. The hemisphere name. Used to
  construct the names of the annotation and morphometry data files to be
  loaded.

- cortex_only:

  logical, whether to set non-cortex data to NA

## Value

dataframe with concatenated vertex values. Each column contains the
values for one measure, concatenated for all subjects. WARNING: This
dataframe can get large if you have many subjects.

## See also

Other concatination functions:
[`group.concat.measures.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/group.concat.measures.standard.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_optional_data();
   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
   subjects_list = c('subject1', 'subject2');
   cm = group.concat.measures.native(subjects_dir, subjects_list,
    c("thickness", "area"), "lh");
} # }
```
