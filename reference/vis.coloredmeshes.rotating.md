# Visualize a list of colored meshes in a single scene and rotate them, movie-style.

Visualize a list of colored meshes in a single scene and rotate them,
movie-style.

## Usage

``` r
vis.coloredmeshes.rotating(
  coloredmeshes,
  background = "white",
  skip_all_na = TRUE,
  style = "default",
  x = 0,
  y = 0,
  z = 1,
  rpm = 6,
  duration = 10,
  rgloptions = rglo(),
  rglactions = list()
)
```

## Arguments

- coloredmeshes:

  list of coloredmesh. A coloredmesh is a named list as returned by the
  coloredmesh.from.\* functions. It has the entries 'mesh' of type
  tmesh3d, a 'col', which is a color specification for such a mesh.

- background:

  string, background color passed to rgl::bg3d()

- skip_all_na:

  logical, whether to skip (i.e., not render) meshes in the list that
  have the property 'rendner' set to FALSE. Defaults to TRUE.
  Practically, this means that a hemisphere for which the data was not
  given is not rendered, instead of being rendered in a single color.

- style:

  a named list of style parameters or a string specifying an available
  style by name (e.g., 'shiny'). Defaults to 'default', the default
  style.

- x:

  rotation x axis value, passed to
  [`spin3d`](https://dmurdoch.github.io/rgl/dev/reference/spin3d.html).
  Defaults to 0.

- y:

  rotation y axis value, passed to
  [`spin3d`](https://dmurdoch.github.io/rgl/dev/reference/spin3d.html).
  Defaults to 1.

- z:

  rotation z axis value, passed to
  [`spin3d`](https://dmurdoch.github.io/rgl/dev/reference/spin3d.html).
  Defaults to 0.

- rpm:

  rotation rpm value, passed to
  [`spin3d`](https://dmurdoch.github.io/rgl/dev/reference/spin3d.html).
  Defaults to 15.

- duration:

  rotation duration value, passed to
  [`spin3d`](https://dmurdoch.github.io/rgl/dev/reference/spin3d.html).
  Defaults to 20.

- rgloptions:

  option list passed to
  [`par3d`](https://dmurdoch.github.io/rgl/dev/reference/par3d.html).
  Example: rgloptions = list("windowRect"=c(50,50,1000,1000));

- rglactions:

  named list. A list in which the names are from a set of pre-defined
  actions. Defaults to the empty list.

## Value

the list of visualized coloredmeshes
