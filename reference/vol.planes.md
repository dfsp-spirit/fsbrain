# Translate names and indices of planes.

Translate names and indices of 3D image planes. The names only make
sense if the data in the volume is in the default FreeSurfer conformed
orientation.

## Usage

``` r
vol.planes(plane = NULL)
```

## Arguments

- plane:

  NULL, a plane index, or a plane name.

## Value

if `plane` is NULL, all available planes and their indices as a named
list. If `plane` is an integer (a plane index), its name. If `plane` is
an characters string (a plane name), its index.

## See also

Other volume utility:
[`vol.boundary.box()`](https://dfsp-spirit.github.io/fsbrain/reference/vol.boundary.box.md),
[`vol.imagestack()`](https://dfsp-spirit.github.io/fsbrain/reference/vol.imagestack.md),
[`vol.merge()`](https://dfsp-spirit.github.io/fsbrain/reference/vol.merge.md),
[`vol.overlay.colors.from.activation()`](https://dfsp-spirit.github.io/fsbrain/reference/vol.overlay.colors.from.activation.md),
[`vol.slice()`](https://dfsp-spirit.github.io/fsbrain/reference/vol.slice.md)
