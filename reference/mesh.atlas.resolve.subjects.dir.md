# Find a subjects dir that contains a mesh atlas for a subject.

Several locations can contain the data of a template subject like
fsaverage, e.g.\\ the package cache and the subjects dir of a FreeSurfer
installation, and not all of them necessarily contain the mesh atlas.
This function checks the locations in the order in which they are
returned by
[`find.subjectsdir.of`](https://dfsp-spirit.github.io/fsbrain/reference/find.subjectsdir.of.md)
and returns the first one that actually contains the atlas files. If
none contains them, the first location found is returned, so that the
caller can report the missing files.

## Usage

``` r
mesh.atlas.resolve.subjects.dir(
  subject_id,
  atlas = "subcortical",
  surface = "subcortical"
)
```

## Arguments

- subject_id:

  string. The subject identifier.

- atlas:

  string. The atlas name, used to construct the annotation file name.

- surface:

  string. The name of the surface mesh that belongs to the atlas.

## Value

string, the path of a subjects dir.
