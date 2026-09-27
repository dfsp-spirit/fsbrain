# Compute the filepath of a volume file.

Takes a volume specification (a filepath, a name without extension, or
an in-memory volume) and computes the filepath of the volume file on
disk.

## Usage

``` r
vol.find.file(volume, default_dir = NULL)
```

## Arguments

- volume:

  character string or named list. If a character string and the file
  exists, it is interpreted as the volume filepath. If it does not
  exist, it is interpreted as the volume name without extension,
  resolved against `default_dir` (and the current working directory) by
  trying the extensions `.mgz`, `.mgh`, `.nii.gz`, and `.nii` (in that
  order). If a named list, it is interpreted as an in-memory volume (see
  [`subject.vol2surf`](https://dfsp-spirit.github.io/fsbrain/reference/subject.vol2surf.md))
  and `NULL` is returned.

- default_dir:

  character string or NULL, the directory to resolve volume names
  against, typically the `mri` directory of a subject.

## Value

character string, the volume filepath, or `NULL` if `volume` is an
in-memory volume.
