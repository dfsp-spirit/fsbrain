# Apply a label to morphometry data.

This function will set all values in morphdata which are *not* part of
the labeldata to NA (or whatever is specified by 'masked_data_value').
This is typically used to ignore values which are not part of the cortex
(or any other label) during your analysis.

## Usage

``` r
apply.labeldata.to.morphdata(morphdata, labeldata, masked_data_value = NA)
```

## Arguments

- morphdata:

  numerical vector, the morphometry data for one hemisphere

- labeldata:

  integer vector or `fs.label` instance. A label as returned by
  [`subject.label`](https://dfsp-spirit.github.io/fsbrain/reference/subject.label.md).

- masked_data_value:

  numerical, the value to set for all morphometry data values of
  vertices which are *not* part of the label. Defaults to NA.

## Value

numerical vector, the masked data.

## See also

Other label functions:
[`apply.label.to.morphdata()`](https://dfsp-spirit.github.io/fsbrain/reference/apply.label.to.morphdata.md),
[`subject.lobes()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.lobes.md),
[`subject.mask()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.mask.md),
[`vis.labeldata.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.labeldata.on.subject.md),
[`vis.subject.label()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.label.md)

Other morphometry data functions:
[`apply.label.to.morphdata()`](https://dfsp-spirit.github.io/fsbrain/reference/apply.label.to.morphdata.md),
[`group.morph.native()`](https://dfsp-spirit.github.io/fsbrain/reference/group.morph.native.md),
[`group.morph.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/group.morph.standard.md),
[`subject.morph.native()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.morph.native.md),
[`subject.morph.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.morph.standard.md)
