# Collect the scimesh line layers of all fs.coloredpaths instances in a renderable list

Walks a renderable list (a flat list of renderables, a hemilist, or a
single renderable) and converts everything that is an fs.coloredpaths
instance to scimesh line layers. Non-line renderables are ignored, they
are handled by
[`coloredmeshes_to_scimesh`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmeshes_to_scimesh.md).

## Usage

``` r
renderables_to_line_layers(renderables, style = "default")
```

## Arguments

- renderables:

  a renderable, or a (possibly nested) list of renderables.

- style:

  a rendering style, see
  [`get.rglstyle`](https://dfsp-spirit.github.io/fsbrain/reference/get.rglstyle.md).

## Value

a list of scimesh line layers, possibly empty.
