# Spread the values in the region_value_list and return them for one hemisphere.

Given an atlas and a list that contains one value for each atlas region,
create morphometry data in which all region vertices are assigned the
value. Can be used to plot stuff like p-values or effect sizes onto
brain regions.

## Usage

``` r
spread.values.over.subject(
  subjects_dir,
  subject_id,
  atlas,
  lh_region_value_list,
  rh_region_value_list,
  value_for_unlisted_regions = NaN,
  silent = FALSE
)
```

## Arguments

- subjects_dir, :

  string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the
  data for all your subjects, each in a subdir named after the subject
  identifier.

- subject_id, :

  string. The subject identifier

- atlas, :

  string. The atlas name. E.g., "aparc", "aparc.2009s", or
  "aparc.DKTatlas". Used to construct the name of the annotation file to
  be loaded.

- lh_region_value_list, :

  named list. A list in which the names are atlas regions, and the
  values are the value to write to all vertices of that region. Applied
  to the left hemisphere.

- rh_region_value_list, :

  named list. A list in which the names are atlas regions, and the
  values are the value to write to all vertices of that region. Applied
  to the right hemisphere.

- value_for_unlisted_regions, :

  numeric scalar. The value to assign to vertices which are part of
  atlas regions that are not listed in region_value_list. Defaults to
  NaN.

- silent:

  logical, whether to suppress mapping info in case of unnamed region
  value lists (see 'lh_region_value_list' description).

## Value

named list with entries 'lh' and 'rh'. Each value is a numeric vector
containing the data for the respective hemisphere.

## See also

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
[`subject.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.annot.md),
[`subject.atlas.agg()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.atlas.agg.md),
[`subject.label.from.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.label.from.annot.md),
[`subject.lobes()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.lobes.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_optional_data();
   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
   lh_region_value_list = list("bankssts"=0.9, "precuneus"=0.7);
   rh_region_value_list = list("bankssts"=0.5);
   morph_like_data =
   spread.values.over.subject(subjects_dir, 'subject1', 'aparc',
   lh_region_value_list, rh_region_value_list);
} # }
```
