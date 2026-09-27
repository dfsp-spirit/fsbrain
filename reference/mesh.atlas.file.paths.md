# Get the file paths of a mesh atlas for a subject.

Compute the paths of the surface mesh and annotation files of a mesh
atlas. The files are not required to exist, see
[`mesh.atlas.check.files`](https://dfsp-spirit.github.io/fsbrain/reference/mesh.atlas.check.files.md).

## Usage

``` r
mesh.atlas.file.paths(subjects_dir, subject_id, atlas, surface)
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

vector of strings, the file paths of the atlas files.
