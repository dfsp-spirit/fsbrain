# Convert a hemilist of fs.coloredmeshes to a list of scimesh mesh descriptors

Convert a hemilist of fs.coloredmeshes to a list of scimesh mesh
descriptors

## Usage

``` r
coloredmeshes_to_scimesh(coloredmeshes, style = "default")
```

## Arguments

- coloredmeshes:

  a named list with entries "lh" and/or "rh", each an fs.coloredmesh
  instance.

- style:

  a rendering style (see
  [`get.rglstyle`](https://dfsp-spirit.github.io/fsbrain/reference/get.rglstyle.md)),
  passed through to `coloredmesh_to_scimesh`.

## Value

a named list of scimesh mesh descriptors, with the same hemilist
structure. Only meshes with `render=TRUE` are included.
