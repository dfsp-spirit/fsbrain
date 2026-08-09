# Split morph data vector at hemisphere boundary.

Given a single vector of per-vertex data for a mesh, split it at the
hemi boundary. This is achieved by loading the respective surface and
checking the number of vertices for the 2 hemispheres.

## Usage

``` r
vdata.split.by.hemi(
  subjects_dir,
  subject_id,
  vdata,
  surface = "white",
  expand = TRUE
)
```

## Arguments

- subjects_dir:

  string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the
  data for all your subjects, each in a subdir named after the subject
  identifier.

- subject_id:

  string. The subject identifier

- vdata:

  numerical vector of data for both hemispheres, one value per vertex

- surface:

  the surface to load to determine the vertex counts

- expand:

  logical, whether to allow input of length 1, and expand (repeat) it to
  the length of the hemispheres.

## Value

a hemilist, each entry contains the data part of the respective
hemisphere.

## Note

Instead of calling this function to split the data, you could use the
'split_by_hemi' parameter of
[`subject.morph.native`](https://dfsp-spirit.github.io/fsbrain/reference/subject.morph.native.md).
