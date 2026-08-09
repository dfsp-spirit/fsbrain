# Visualize a list of colored meshes in a single scene.

Visualize a list of colored meshes in a single scene.

## Usage

``` r
vis.coloredmeshes(
  coloredmeshes,
  background = "white",
  skip_all_na = TRUE,
  style = "default",
  rgloptions = rglo(),
  rglactions = list(),
  draw_colorbar = FALSE
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
  have the property 'render' set to FALSE. Defaults to TRUE.
  Practically, this means that a hemisphere for which the data was not
  given is not rendered, instead of being rendered in a single color.

- style:

  a named list of style parameters or a string specifying an available
  style by name (e.g., 'shiny'). Defaults to 'default', the default
  style.

- rgloptions:

  option list passed to
  [`par3d`](https://dmurdoch.github.io/rgl/dev/reference/par3d.html).
  Example: `rgloptions = list("windowRect"=c(50,50,1000,1000))`;

- rglactions:

  named list. A list in which the names are from a set of pre-defined
  actions. Defaults to the empty list.

- draw_colorbar:

  logical. Whether to draw a colorbar. WARNING: Will only show up if
  there is enough space in the plot area and does not resize properly.
  Defaults to FALSE. See
  [`coloredmesh.plot.colorbar.separate`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.plot.colorbar.separate.md)
  for an alternative.

## Value

the list of visualized coloredmeshes

## Note

To change or adapt the colorbar, you should use the makecmap_options
parameter when constructing them in a vis function. See the example.

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_optional_data();
   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
   cm = vis.subject.morph.native(subjects_dir, 'subject1', 'thickness',
    makecmap_options=list('n'=100, 'colFn'=viridis::viridis));
   # You could mess with the meshes here.
   vis.coloredmeshes(cm);
} # }
```
