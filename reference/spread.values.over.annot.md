# Spread a single value for a region to all region vertices.

Given an annotation and a list of values (one per brain region), return
data that has the values for each region mapped to all region vertices.

## Usage

``` r
spread.values.over.annot(
  annot,
  region_value_list,
  value_for_unlisted_regions = NaN,
  warn_on_unmatched_list_regions = FALSE,
  warn_on_unmatched_atlas_regions = FALSE
)
```

## Arguments

- annot, :

  annotation. The result of calling fs.read.annot.

- region_value_list, :

  named list of strings. Each name must be a region name from the
  annotation, and the value must be the value to spread to all region
  vertices.

- value_for_unlisted_regions, :

  numeric scalar. The value to assign to vertices which are part of
  atlas regions that are not listed in region_value_list. Defaults to
  NaN.

- warn_on_unmatched_list_regions, :

  logical. Whether to print a warning when a region occurs in the
  region_value_list that is not part of the given atlas (and the value
  assigned to this region is thus ignored in the output file and data).
  Defaults to FALSE.

- warn_on_unmatched_atlas_regions, :

  logical. Whether to print a warning when a region occurs in the atlas
  that is not part of the given region_value_list (and thus the vertices
  of the region will be assigned the value 'value_for_unlisted_regions'
  in the output file and data). Defaults to FALSE.

## Value

named list with following entries: "spread_data": a vector of length n,
where n is the number of vertices in the annotation. One could write
this to an MGH or curv file for visualization. "regions_not_in_annot":
list of regions which are not in the annotation, but in the
region_value_list. Their values were ignored.

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
   annot = subject.annot(subjects_dir, 'subject1', 'lh', 'aparc');
   region_value_list = list("bankssts"=0.9, "precuneus"=0.7);
   morph_like_data =
   spread.values.over.annot(annot, region_value_list, value_for_unlisted_regions=0.0);
} # }
```
