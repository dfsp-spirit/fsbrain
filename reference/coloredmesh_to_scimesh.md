# Convert a single fs.coloredmesh to a scimesh mesh descriptor

Convert a single fs.coloredmesh to a scimesh mesh descriptor

## Usage

``` r
coloredmesh_to_scimesh(cmesh, style = "default")
```

## Arguments

- cmesh:

  an fs.coloredmesh instance.

- style:

  a rendering style: a style name, a named list of style parameters, or
  'from_mesh' (use cmesh\$style). Only the alpha value is consumed here
  (per-mesh vertex alpha).

## Value

a scimesh mesh descriptor list with vertices, triangles, and colors.
