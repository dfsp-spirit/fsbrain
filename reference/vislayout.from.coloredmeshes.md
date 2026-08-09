# Visualize coloredmeshes from several angles and combine the images into a new figure.

Create a tight layout view of coloredmeshes from several angles. Creates
separate `sd_<angle>` images, then crops and finally merges them into a
single output image with image magick. The `coloredmeshes` to pass to
this function are usually obtained by running any `vis*` function (like
[`vis.subject.morph.native`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.morph.native.md),
[`vis.subject.morph.standard`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.morph.standard.md),
[`vis.subject.label`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.label.md),
[`vis.subject.annot`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.annot.md),
and others). That means you can use this function to visualize all kinds
of data, e.g., morphometry data in native and standard space, labels,
and brain atlases.

## Usage

``` r
vislayout.from.coloredmeshes(
  coloredmeshes,
  view_angles = get.view.angle.names(angle_set = "t4"),
  rgloptions = rglo(),
  rglactions = list(),
  style = "default",
  output_img = "fsbrain_arranged.png",
  silent = FALSE,
  grid_like = TRUE,
  background_color = "white",
  transparency_color = NULL
)
```

## Arguments

- coloredmeshes, :

  list of coloredmesh. A coloredmesh is a named list as returned by the
  `coloredmesh.from*` functions (like
  [`coloredmesh.from.morph.native`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.from.morph.native.md)).
  It has the entries 'mesh' of type tmesh3d, a 'col', which is a color
  specification for such a mesh. The `vis*` functions (like
  [`vis.subject.morph.native`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.morph.native.md))
  all return a list of coloredmeshes.

- view_angles:

  list of strings. See
  [`get.view.angle.names`](https://dfsp-spirit.github.io/fsbrain/reference/get.view.angle.names.md)
  for all valid strings.

- rgloptions:

  option list passed to
  [`par3d`](https://dmurdoch.github.io/rgl/dev/reference/par3d.html).
  Example: `rgloptions = list("windowRect"=c(50,50,1000,1000))`.

- rglactions:

  named list. A list in which the names are from a set of pre-defined
  actions. The values can be used to specify parameters for the action.

- style:

  character string, a rendering style, e.g., 'default', 'shiny' or
  'semitransparent'. Alternatively, a named list of style parameters
  (see
  [`material3d`](https://dmurdoch.github.io/rgl/dev/reference/material.html)),
  e.g., `list("shininess"=50, specular="black", alpha=0.5)`. Use the
  magic word 'from_mesh' to use the 'style' field of each coloredmesh
  instead of a single, global style. In that case, you will have to make
  sure your meshes have such a field, if not, the style 'default' is
  used as a fallback for those which don't.

- output_img:

  string, path to the output file. Defaults to "fsbrain_arranged.png"

- silent:

  logical, whether to suppress all messages

- grid_like:

  logical, whether to arrange the images in a grid-like fashion. If
  FALSE, they will all be merged horizontally. Passed to
  [`arrange.brainview.images`](https://dfsp-spirit.github.io/fsbrain/reference/arrange.brainview.images.md).

- background_color:

  hex color string (like '#FFFFFF'), the color to use for the
  background. Ignored if 'transparency_color' is not NULL. To get a
  transparent background, use 'transparency_color' instead of this
  parameter. WARNING: Do not use color names (like 'gray'), as their
  interpretation differs between rgl and image magick!

- transparency_color:

  hex color string (like '#FFFFFF'), the temporary background color that
  will get mapped to transparency, or NULL if you do not want a
  transparent background. If used, it can be any color that does not
  occur in the foreground. Try '#FFFFFF' (white) or '#000000' (black) if
  in doubt. WARNING: Do not use color names (like 'gray'), as their
  interpretation differs between rgl and image magick!

## Value

named list, see
[`arrange.brainview.images`](https://dfsp-spirit.github.io/fsbrain/reference/arrange.brainview.images.md)
for details

## See also

Other visualization functions:
[`highlight.vertices.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/highlight.vertices.on.subject.md),
[`highlight.vertices.on.subject.spheres()`](https://dfsp-spirit.github.io/fsbrain/reference/highlight.vertices.on.subject.spheres.md),
[`vis.color.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.color.on.subject.md),
[`vis.data.on.fsaverage()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.data.on.fsaverage.md),
[`vis.data.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.data.on.subject.md),
[`vis.labeldata.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.labeldata.on.subject.md),
[`vis.mask.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.mask.on.subject.md),
[`vis.region.values.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.region.values.on.subject.md),
[`vis.rglwidget()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.rglwidget.md),
[`vis.subject.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.annot.md),
[`vis.subject.label()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.label.md),
[`vis.subject.morph.native()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.morph.native.md),
[`vis.subject.morph.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.morph.standard.md),
[`vis.subject.pre()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.pre.md),
[`vis.symmetric.data.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.symmetric.data.on.subject.md),
[`vis.volume.on.surface()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.volume.on.surface.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_optional_data();
   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");
   # Use any vis function to get coloredmeshes. You can visualize morphometry,
   #  labels, an atlas, whatever. You can suppress the view unless you need it.
   coloredmeshes = vis.subject.morph.native(subjects_dir, "subject1", "thickness",
    cortex_only=TRUE, rglactions=list("clip_data"=c(0.05, 0.95)),
    views=NULL);
   # The meshes contain the surface, data, and color information and can be
   #  visualized. You could adapt the rendering style while doing so:
   vislayout.from.coloredmeshes(coloredmeshes, style='shiny');
   # You could change the rendering style on a per-mesh basis.
   coloredmeshes[[1]]$style = list("shininess"=50, alpha=0.5);
   vislayout.from.coloredmeshes(coloredmeshes, style='from_mesh');
} # }

```
