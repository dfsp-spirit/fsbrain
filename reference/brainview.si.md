# Visualize a list of colored meshes from a single viewpoint, interactively.

Visualize a list of colored meshes from a single viewpoint,
interactively.

## Usage

``` r
brainview.si(
  coloredmeshes,
  background = "white",
  skip_all_na = TRUE,
  style = "default",
  draw_labels = FALSE,
  rgloptions = rglo(),
  rglactions = list(),
  draw_colorbar = FALSE
)
```

## Arguments

- coloredmeshes, :

  list of coloredmesh. A coloredmesh is a named list as returned by the
  coloredmesh.from.\* functions. It has the entries 'mesh' of type
  tmesh3d, a 'col', which is a color specification for such a mesh.

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

- draw_labels:

  logical, whether to draw label text for the different views that show
  information on the view direction and hemisphere displayed in a
  subplot. Defaults to FALSE.

- rgloptions, :

  named list. Parameters passed to
  [`par3d`](https://dmurdoch.github.io/rgl/dev/reference/par3d.html).
  Defaults to the empty list.

- rglactions, :

  named list. A list in which the names are from a set of pre-defined
  actions. Defaults to the empty list.

- draw_colorbar:

  logical, whether to draw a colorbar. WARNING: The colorbar is drawn to
  a subplot, and this only works if there is enough space for it. You
  will have to increase the plot size using the 'rlgoptions' parameter
  for the colorbar to show up. Defaults to FALSE.
