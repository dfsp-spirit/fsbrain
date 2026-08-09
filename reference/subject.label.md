# Retrieve label data for a single subject.

Load a label (like 'label/lh.cortex.label') for a subject from disk.
Uses knowledge about the FreeSurfer directory structure to load the
correct file.

## Usage

``` r
subject.label(
  subjects_dir,
  subject_id,
  label,
  hemi,
  return_one_based_indices = TRUE,
  full = FALSE
)
```

## Arguments

- subjects_dir:

  string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the
  data for all your subjects, each in a subdir named after the subject
  identifier.

- subject_id:

  string. The subject identifier

- label:

  string. Name of the label file, without the hemi part. You can include
  the '.label' suffix. E.g., 'cortex.label' for '?h.cortex.label'. You
  can also pass just the label (e.g., 'cortex'): if the string does not
  end with the suffix '.label', that suffix gets added auomatically.

- hemi:

  string, one of 'lh', 'rh', or 'both'. The hemisphere name. Used to
  construct the names of the label data files to be loaded. For 'both',
  see the information on the return value.

- return_one_based_indices:

  logical. Whether the indices should be 1-based. Indices are stored
  zero-based in the file, but R uses 1-based indices. Defaults to TRUE,
  which means that 1 will be added to all indices read from the file
  before returning them.

- full:

  logical, whether to return the full label structure instead of only
  the vertex indices.

## Value

integer vector with label data: the list of vertex indices in the label.
See 'return_one_based_indices' for important information. If parameter
`hemi` is set to `both`, a named list with entries `lh` and `rh` is
returned, and the values of are the respective labels.

## See also

Other label data functions:
[`group.label()`](https://dfsp-spirit.github.io/fsbrain/reference/group.label.md),
[`labeldata.from.mask()`](https://dfsp-spirit.github.io/fsbrain/reference/labeldata.from.mask.md),
[`mask.from.labeldata.for.hemi()`](https://dfsp-spirit.github.io/fsbrain/reference/mask.from.labeldata.for.hemi.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_optional_data();
   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
   cortex_lh = subject.label(subjects_dir, "subject1", "cortex.label", "lh");
} # }
```
