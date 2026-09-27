# Project a volume onto the vertices of the given surfaces (internal workhorse).

Project a volume onto the vertices of the given surfaces (internal
workhorse).

## Usage

``` r
vol.vol2surf.hemilist(
  surfaces,
  volume,
  default_dir,
  interpolation,
  frame,
  clamp,
  check_fov,
  affine = NULL,
  vox2ras = "auto"
)
```

## Arguments

- surfaces:

  named list of vertex coordinate matrices, per hemisphere.

- volume:

  the volume, as accepted by
  [`vol.load`](https://dfsp-spirit.github.io/fsbrain/reference/vol.load.md).

- default_dir:

  character string or NULL, the directory used to resolve volume names
  without extension.

- interpolation:

  character string, see
  [`subject.vol2surf`](https://dfsp-spirit.github.io/fsbrain/reference/subject.vol2surf.md).

- frame:

  integer scalar, see
  [`subject.vol2surf`](https://dfsp-spirit.github.io/fsbrain/reference/subject.vol2surf.md).

- clamp:

  logical, whether to clamp query points outside the volume to the
  volume border.

- check_fov:

  logical, whether to check (and warn about) vertices outside the
  volume.

- affine:

  numeric 4x4 matrix or `NULL`, used instead of the affine matrix from
  the volume header if given.

- vox2ras:

  character string, see
  [`subject.vol2surf`](https://dfsp-spirit.github.io/fsbrain/reference/subject.vol2surf.md).

## Value

named list of numeric vectors (one per hemisphere), with attribute
'outside' (named list of logical vectors or NULLs).
