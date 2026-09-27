# Check whether all regions of a mesh atlas visualization are hidden.

Regions are hidden by assigning the value NaN to them (see
[`vis.subcortical.region.values`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subcortical.region.values.md)).
This function determines whether that leaves any region to render, so
that the caller can stop with a helpful message instead of an unhelpful
error from the (unrelated) data processing code. Regions which are not
listed in the region value lists are hidden if
`value_for_unlisted_regions` is NaN as well, which requires the atlas to
be read to know the region names.

## Usage

``` r
mesh.atlas.all.regions.hidden(
  subjects_dir,
  subject_id,
  atlas,
  lh_region_value_list,
  rh_region_value_list,
  value_for_unlisted_regions
)
```

## Arguments

- subjects_dir:

  string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the
  data for all your subjects, each in a subdir named after the subject
  identifier.

- subject_id:

  string. The subject identifier.

- atlas:

  string. The atlas name, used to construct the annotation file name.

- lh_region_value_list:

  named list or NULL, the region values for the left hemisphere.

- rh_region_value_list:

  named list or NULL, the region values for the right hemisphere.

- value_for_unlisted_regions:

  numerical scalar, the value assigned to regions which do not occur in
  the region value lists.

## Value

logical, TRUE if none of the regions of the atlas would be rendered
(because all their values are NaN), FALSE otherwise.
