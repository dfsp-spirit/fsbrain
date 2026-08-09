# Show one or more views of the given meshes in rgl windows.

Show one or more views of the given meshes in rgl windows.

## Usage

``` r
brainviews(
  views,
  coloredmeshes,
  rgloptions = rglo(),
  rglactions = list(),
  style = "default",
  draw_colorbar = FALSE,
  background = "white"
)
```

## Arguments

- views:

  list of strings. Valid entries include: 'si': single interactive view.
  `'sd_<angle>'`: single view from angle `<angle>`. The `<angle>` part
  must be one of the strings returned by
  [`get.view.angle.names`](https://dfsp-spirit.github.io/fsbrain/reference/get.view.angle.names.md).
  Example: 'sd_caudal'. 'sr': single rotating view. 't4': tiled view
  showing the brain from 4 angles. 't9': tiled view showing the brain
  from 9 angles.

- coloredmeshes:

  list of coloredmesh or renderable. A coloredmesh is a named list as
  returned by the coloredmesh.from.\* functions. It has the entries
  'mesh' of type tmesh3d, a 'col', which is a color specification for
  such a mesh.

- rgloptions:

  option list passed to
  [`par3d`](https://dmurdoch.github.io/rgl/dev/reference/par3d.html).
  Example: `rgloptions = list("windowRect"=c(50,50,1000,1000))`

- rglactions:

  named list. A list in which the names are from a set of pre-defined
  actions. The values can be used to specify parameters for the action.

- style:

  character string, a rendering style, e.g., 'default', 'shiny' or
  'semitransparent'.

- draw_colorbar:

  logical, whether to draw a colorbar. WARNING: The colorbar is drawn to
  a subplot, and this only works if there is enough space for it. You
  will have to increase the plot size using the 'rlgoptions' parameter
  for the colorbar to show up. Defaults to FALSE. See
  [`coloredmesh.plot.colorbar.separate`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.plot.colorbar.separate.md)
  for an alternative.

- background:

  the background color for the visualization, e.g., 'white' or
  '#FF0000'. Note that alpha/transparency is not supported by rgl.

## Value

list of coloredmeshes. The coloredmeshes used for the visualization.

## See also

[`get.view.angle.names`](https://dfsp-spirit.github.io/fsbrain/reference/get.view.angle.names.md)
