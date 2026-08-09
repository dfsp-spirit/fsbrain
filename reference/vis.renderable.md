# Visualize a renderable object

Renders instances of `coloredmesh`, `coloredvoxels` and `Triangles3D`.

## Usage

``` r
vis.renderable(cmesh, skip_all_na = TRUE, style = "default")
```

## Arguments

- cmesh:

  an instance of one of the supported renderable classes

- skip_all_na:

  logical, whether to skip rendering hidden instances

- style:

  a rendering style, can be a style name or a list defining an rgl
  material style

## See also

[`fsbrain.renderable`](https://dfsp-spirit.github.io/fsbrain/reference/fsbrain.renderable.md)
