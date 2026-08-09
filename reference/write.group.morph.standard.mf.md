# Write per-vertex standard space data for a group of subjects to given file names.

Write per-vertex standard space data for a group of subjects to given
file names.

## Usage

``` r
write.group.morph.standard.mf(
  filepaths_hl,
  data_hl,
  format = "auto",
  create_dirs = TRUE
)
```

## Arguments

- filepaths_hl:

  [`hemilist`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.md)
  of vectors of character strings, the full paths to the output files,
  including file names and extension.

- data_hl:

  [`hemilist`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.md)
  of numerical matrix or data.frame, the morph data for the hemispheres
  of all subjects. See `groupmorph.split.hemilist` to get this format if
  you have a full matrix or dataframe for both hemispheres.

- format:

  character string, a valid format spec for
  [`freesurferformats::write.fs.morph`](https://rdrr.io/pkg/freesurferformats/man/write.fs.morph.html),
  e.g., "auto" to derive from filename, "mgh", "mgz", "curv" or others.

- create_dirs:

  logical, whether to create missing (sub) directories which occur in
  the 'filepaths'.

## See also

[`write.group.morph.standard.sf`](https://dfsp-spirit.github.io/fsbrain/reference/write.group.morph.standard.sf.md)
to write the data to a single stacked file instead.
