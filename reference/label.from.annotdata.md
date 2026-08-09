# Extract a region from an annotation as a label.

The returned label can be used to mask morphometry data, e.g., to set
the values of a certain region to `NaN` or to extract only values from a
certain region.

## Usage

``` r
label.from.annotdata(
  annotdata,
  region,
  return_one_based_indices = TRUE,
  invert = FALSE,
  error_on_invalid_region = TRUE
)
```

## Arguments

- annotdata, :

  annotation. An annotation for one hemisphere, as returned by
  [`subject.annot`](https://dfsp-spirit.github.io/fsbrain/reference/subject.annot.md).
  This must be the loaded data, not a path to a file.

- region, :

  string. A valid region name for the annotation, i.e., one of the
  regions of the atlas used to create the annotation.

- return_one_based_indices, :

  logical. Whether the indices should be 1-based. Indices are stored
  zero-based in label files, but R uses 1-based indices. Defaults to
  TRUE.

- invert, :

  logical. If TRUE, return the indices of all vertices which are NOT
  part of the region. Defaults to FALSE.

- error_on_invalid_region, :

  logical. Whether to throw an error if the given region does not appear
  in the region list of the annotation. If set to FALSE, this will be
  ignored and an empty vertex list will be returned. Defaults to TRUE.

## Value

integer vector with label data: the list of vertex indices in the label.
See 'return_one_based_indices' for important information.

## See also

Other atlas functions:
[`get.atlas.region.names()`](https://dfsp-spirit.github.io/fsbrain/reference/get.atlas.region.names.md),
[`group.agg.atlas.native()`](https://dfsp-spirit.github.io/fsbrain/reference/group.agg.atlas.native.md),
[`group.agg.atlas.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/group.agg.atlas.standard.md),
[`group.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/group.annot.md),
[`group.label.from.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/group.label.from.annot.md),
[`label.to.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/label.to.annot.md),
[`regions.to.ignore()`](https://dfsp-spirit.github.io/fsbrain/reference/regions.to.ignore.md),
[`spread.values.over.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/spread.values.over.annot.md),
[`spread.values.over.hemi()`](https://dfsp-spirit.github.io/fsbrain/reference/spread.values.over.hemi.md),
[`spread.values.over.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/spread.values.over.subject.md),
[`subject.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.annot.md),
[`subject.atlas.agg()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.atlas.agg.md),
[`subject.label.from.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.label.from.annot.md),
[`subject.lobes()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.lobes.md)
