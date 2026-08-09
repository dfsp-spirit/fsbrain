# Split a per-vertex group data matrix for both hemispheres into a hemilist at given index.

Split a per-vertex group data matrix for both hemispheres into a
hemilist at given index.

## Usage

``` r
groupmorph.split.hemilist(data, numverts_lh)
```

## Arguments

- data:

  numerical matrix or dataframe of per-vertex data, with subjects in
  columns

- numverts_lh:

  scalar positive integer, the number of vertices in the left hemisphere
  mesh (defining the index where to split).

## Value

[`hemilist`](https://dfsp-spirit.github.io/fsbrain/reference/hemilist.md)
of the data, split at the index.

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_optional_data();
   fsbrain::download_fsaverage(TRUE);
   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
   subjects_list = c("subject1", "subject2");
   data = group.morph.standard(subjects_dir, subjects_list, "thickness", "lh", fwhm='10');
   numverts_lh = subject.num.verts(subjects_dir, "fsaverage", hemi="lh");
   data_hemilist = groupmorph.split.hemilist(data, numverts_lh);
} # }
```
