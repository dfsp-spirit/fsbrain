# Visualize a list of colored meshes from a single defined angle.

Visualize a list of colored meshes from a single defined angle.

## Usage

``` r
brainview.sd(
  coloredmeshes,
  view_angle,
  background = "white",
  skip_all_na = TRUE,
  style = "default",
  rgloptions = rglo(),
  rglactions = list(),
  draw_colorbar = FALSE
)
```

## Arguments

- coloredmeshes, :

  list of coloredmesh. A coloredmesh is a named list as returned by the
  `coloredmesh.from*` functions (like
  [`coloredmesh.from.morph.native`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.morph.native.md)).
  It has the entries 'mesh' of type tmesh3d, a 'col', which is a color
  specification for such a mesh. Note that the `vis*` functions (like
  [`vis.subject.morph.native`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.morph.native.md))
  all return a list of coloredmeshes.

- view_angle:

  character string, the view angle. One of 'lateral_lh', 'dorsal',
  'lateral_rh', 'medial_lh', 'ventral', 'medial_rh', 'rostral' or
  'caudal'. See
  [`get.view.angle.names`](https://dfsp-spirit.github.io/fsbrain/reference/get.view.angle.names.md).

- background:

  string, background color passed to
  [`bg3d`](https://dmurdoch.github.io/rgl/dev/reference/bg.html).

- skip_all_na:

  logical, whether to skip (i.e., not render) meshes in the list that
  have the property 'render' set to FALSE. Defaults to TRUE.
  Practically, this means that a hemisphere for which the data was not
  given is not rendered, instead of being rendered in a single color.

- style, :

  a named list of style parameters or a string specifying an available
  style by name (e.g., 'shiny'). Defaults to 'default', the default
  style.

- rgloptions, :

  named list. Parameters passed to
  [`par3d`](https://dmurdoch.github.io/rgl/dev/reference/par3d.html).
  Defaults to the empty list. To increase plot resolution to 2000x1600
  px, try: `rgloptions=list("windowRect"=c(50,50,2000,1600))`.

- rglactions, :

  named list. A list in which the names are from a set of pre-defined
  actions. The values can be used to specify parameters for the action.

- draw_colorbar:

  logical, whether to draw a colorbar. WARNING: The colorbar is drawn to
  a subplot, and this only works if there is enough space for it. You
  will have to increase the plot size using the 'rlgoptions' parameter
  for the colorbar to show up. Defaults to FALSE.
