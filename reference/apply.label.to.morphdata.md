# Load a label from file and apply it to morphometry data.

This function will set all values in morphdata which are *not* part of
the label loaded from the file to NA (or whatever is specified by
'masked_data_value'). This is typically used to ignore values which are
not part of the cortex (or any other label) during your analysis.

## Usage

``` r
apply.label.to.morphdata(
  morphdata,
  subjects_dir,
  subject_id,
  hemi,
  label,
  masked_data_value = NA
)
```

## Arguments

- morphdata:

  numerical vector, the morphometry data for one hemisphere

- subjects_dir:

  string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the
  data for all your subjects, each in a subdir named after the subject
  identifier.

- subject_id:

  string. The subject identifier

- hemi:

  string, one of 'lh', 'rh' or 'both'. The hemisphere name. Used to
  construct the names of the annotation and morphometry data files to be
  loaded.

- label:

  string, `fs.label` instance, or label vertex data. If a string,
  interpreted as the file name of the label file, without the hemi part
  (if any), optionally including the '.label' suffix. E.g.,
  'cortex.label' or 'cortex' for '?h.cortex.label'.

- masked_data_value:

  numerical, the value to set for all morphometry data values of
  vertices which are *not* part of the label. Defaults to NA.

## Value

numerical vector, the masked data.

## See also

Other label functions:
[`apply.labeldata.to.morphdata()`](https://dfsp-spirit.github.io/fsbrain/reference/apply.labeldata.to.morphdata.md),
[`subject.lobes()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.lobes.md),
[`subject.mask()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.mask.md),
[`vis.labeldata.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.labeldata.on.subject.md),
[`vis.subject.label()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.label.md)

Other morphometry data functions:
[`apply.labeldata.to.morphdata()`](https://dfsp-spirit.github.io/fsbrain/reference/apply.labeldata.to.morphdata.md),
[`group.morph.native()`](https://dfsp-spirit.github.io/fsbrain/reference/group.morph.native.md),
[`group.morph.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/group.morph.standard.md),
[`subject.morph.native()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.morph.native.md),
[`subject.morph.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.morph.standard.md)
