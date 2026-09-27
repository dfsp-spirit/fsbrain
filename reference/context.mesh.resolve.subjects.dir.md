# Resolve the subjects dir for a context mesh.

Determine the subjects dir from which the requested context mesh can be
loaded, if the user did not pass one. For the template subject
'fsaverage' the surfaces are downloaded into the package cache if they
are not available, for other subjects the standard search locations are
checked, see
[`find.subjectsdir.of`](https://dfsp-spirit.github.io/fsbrain/reference/find.subjectsdir.of.md).

## Usage

``` r
context.mesh.resolve.subjects.dir(subject_id = "fsaverage", context = NULL)
```

## Arguments

- subject_id:

  string, the subject identifier.

- context:

  the context mesh definition, as passed by the user, see
  [`vis.volume.clusters`](https://dfsp-spirit.github.io/fsbrain/reference/vis.volume.clusters.md).
  A subject id and subjects dir can be part of this definition, in which
  case they take precedence.

## Value

string, the path to the subjects dir to use, or `NULL` if the context
mesh does not have to be loaded from disk (e.g., because it was passed
as a pre-built mesh).
