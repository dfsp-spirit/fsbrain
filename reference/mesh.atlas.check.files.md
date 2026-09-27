# Check that a mesh atlas is available for a subject.

Check that the surface mesh and the annotation files of a mesh atlas
exist for a subject, and stop with a helpful error message otherwise.

## Usage

``` r
mesh.atlas.check.files(subjects_dir, subject_id, atlas, surface)
```

## Arguments

- subjects_dir:

  string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the
  data for all your subjects, each in a subdir named after the subject
  identifier.

- subject_id:

  string. The subject identifier.

- atlas:

  string. The atlas name, used to construct the annotation file name.

- surface:

  string. The name of the surface mesh that belongs to the atlas.

## Value

vector of strings, the file paths of the atlas files, invisibly. The
function stops if any of the files is missing.
