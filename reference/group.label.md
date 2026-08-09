# Retrieve label data for a group of subjects.

Load a label (like 'label/lh.cortex.label') for a group of subjects from
disk. Uses knowledge about the FreeSurfer directory structure to load
the correct file.

## Usage

``` r
group.label(
  subjects_dir,
  subjects_list,
  label,
  hemi,
  return_one_based_indices = TRUE
)
```

## Arguments

- subjects_dir, :

  string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the
  data for all your subjects, each in a subdir named after the subject
  identifier.

- subjects_list, :

  vector of strings. The subject identifiers.

- label, :

  string. Name of the label file, without the hemi part (if any), but
  including the '.label' suffix. E.g., 'cortex.label' for
  '?h.cortex.label'

- hemi, :

  string, one of 'lh' or 'rh'. The hemisphere name. Used to construct
  the names of the label data files to be loaded.

- return_one_based_indices, :

  logical. Whether the indices should be 1-based. Indices are stored
  zero-based in the file, but R uses 1-based indices. Defaults to TRUE,
  which means that 1 will be added to all indices read from the file
  before returning them.

## Value

named list of integer vectors with label data: Each name is a subject
identifier from subjects_list, and the values are lists of the vertex
indices in the respective label. See 'return_one_based_indices' for
important information.

## See also

Other label data functions:
[`labeldata.from.mask()`](https://dfsp-spirit.github.io/fsbrain/reference/labeldata.from.mask.md),
[`mask.from.labeldata.for.hemi()`](https://dfsp-spirit.github.io/fsbrain/reference/mask.from.labeldata.for.hemi.md),
[`subject.label()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.label.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_optional_data();
   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
   subjects_list = c("subject1", "subject2");
   labels = group.label(subjects_dir, subjects_list, 'cortex.label', "lh");
} # }
```
