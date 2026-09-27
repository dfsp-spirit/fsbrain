# Check whether a subject directory contains the essential files of a subject.

A subject directory can be incomplete, e.g., the package cache can
contain a subject directory that was created by downloading only the
atlas files for it (see
[`download_fsaverage_atlases`](https://dfsp-spirit.github.io/fsbrain/reference/download_fsaverage_atlases.md)).
Such a directory does not contain the surfaces of the subject, and using
it makes all functions fail that need them. This function checks for the
file 'surf/lh.white', which is part of every subject (including
fsaverage and the fsaverage template derivatives).

## Usage

``` r
subject.dir.has.core.files(subject_dir)
```

## Arguments

- subject_dir:

  string, the path to the directory of a single subject.

## Value

logical, whether the directory contains the essential files of a
subject.
