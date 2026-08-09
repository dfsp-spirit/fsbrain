# Write single hemi per-vertex data for a group of subjects to given file names.

Write single hemi per-vertex data for a group of subjects to given file
names.

## Usage

``` r
write.group.morph.standard.singlehemi(
  filepaths,
  data,
  format = "auto",
  create_dirs = TRUE
)
```

## Arguments

- filepaths:

  vector of character strings, the full paths to the output files,
  including file names and extension.

- data:

  numerical matrix or data.frame, the morph data for a single hemi (as
  returned by `group.morph.standard`). Number of subjects (columns) must
  match the length of the 'filepaths'.

- format:

  character string, a valid format spec for
  [`freesurferformats::write.fs.morph`](https://rdrr.io/pkg/freesurferformats/man/write.fs.morph.html),
  e.g., "auto" to derive from filename, "mgh", "mgz", "curv" or others.

- create_dirs:

  logical, whether to create missing (sub) directories which occur in
  the 'filepaths'.
