# Determine atlas region names from a subject.

Determine atlas region names from a subject. WARNING: Not all subjects
have all regions of an atlas. You should use an average subject like
fsaverage to get all regions.

## Usage

``` r
get.atlas.region.names(
  atlas,
  template_subjects_dir = NULL,
  template_subject = "fsaverage",
  hemi = "lh"
)
```

## Arguments

- atlas, :

  string. The atlas name. E.g., "aparc", "aparc.2009s", or
  "aparc.DKTatlas". Used to construct the name of the annotation file to
  be loaded.

- template_subjects_dir, :

  string. The directory containing the dir of the template_subject.
  E.g., the path to FREESURFER_HOME/subjects. If NULL, env vars will be
  searched for candidates, and the function will fail if they are not
  set correctly. Defaults to NULL.

- template_subject, :

  string. The subject identifier. Defaults to 'fsaverage'.

- hemi, :

  string, one of 'lh' or 'rh'. The hemisphere name. Used to construct
  the names of the annotation and morphometry data files to be loaded.
  Defaults to 'lh'. Should not matter much, unless you do not have the
  file for one of the hemis for some reason.

## Value

vector of strings, the region names.

## See also

Other atlas functions:
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
[`subject.label.from.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.label.from.annot.md),
[`subject.lobes()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.lobes.md)

## Examples

``` r
if (FALSE) { # \dontrun{
 fsbrain::download_optional_data();
 subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
 atlas_regions = get.atlas.region.names('aparc',
 template_subjects_dir=subjects_dir, template_subject='subject1');
} # }
```
