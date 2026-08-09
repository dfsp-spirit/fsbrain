# Load labels representing brain lobes.

This gives you labels that represent brain lobes for a subject. The lobe
definition is based on the Desikan-Killiany atlas (Desikan *et al.*,
2010) as suggested on the FreeSurfer website at
https://surfer.nmr.mgh.harvard.edu/fswiki/CorticalParcellation.

## Usage

``` r
subject.lobes(
  subjects_dir,
  subject_id,
  hemi = "both",
  include_cingulate = TRUE,
  as_annot = FALSE
)
```

## Arguments

- subjects_dir:

  string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the
  data for all your subjects, each in a subdir named after the subject
  identifier.

- subject_id:

  string. The subject identifier

- hemi:

  string, one of 'lh', 'rh', or 'both'. The hemisphere name. Used to
  construct the names of the surface file to be loaded. For 'both', see
  the information on the return value.

- include_cingulate:

  logical, whether to include the vertices of the cingulate in the lobes

- as_annot:

  return a hemilist of annotations instead of the return value described
  in the *value* section

## Value

hemilist of integer vectors, the vectors represent vertex indices of the
hemispheres, and each vertex is assigned one of the following values:
`0`=no_lobe, `1`=frontal, `2`=parietal, `3`=temporal, `4`=occipital.

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
[`spread.values.over.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/spread.values.over.subject.md),
[`subject.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.annot.md),
[`subject.atlas.agg()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.atlas.agg.md),
[`subject.label.from.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.label.from.annot.md)

Other label functions:
[`apply.label.to.morphdata()`](https://dfsp-spirit.github.io/fsbrain/reference/apply.label.to.morphdata.md),
[`apply.labeldata.to.morphdata()`](https://dfsp-spirit.github.io/fsbrain/reference/apply.labeldata.to.morphdata.md),
[`subject.mask()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.mask.md),
[`vis.labeldata.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.labeldata.on.subject.md),
[`vis.subject.label()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.label.md)
