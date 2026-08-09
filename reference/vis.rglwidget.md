# Visualize coloredmeshes as an interactive rgl WebGL widget for use in R Shiny apps and RMarkdown documents.

Create an interactive 3D brain view that can be embedded in R Shiny
applications or RMarkdown/HTML documents. Unlike the standard `vis.*`
functions which open an X11/OpenGL window and are designed for static
screenshot export, this function renders to a headless rgl device and
returns an
[`rglwidget`](https://dmurdoch.github.io/rgl/dev/reference/rglwidget.html)
object that provides interactive 3D rendering in a web browser. The user
can rotate, zoom, and pan the brain in the widget.

## Usage

``` r
vis.rglwidget(
  coloredmeshes,
  background = "white",
  skip_all_na = TRUE,
  style = "default",
  rgloptions = rglo(),
  ...
)
```

## Arguments

- coloredmeshes, :

  a hemilist of coloredmeshes (as returned by
  [`vis.subject.morph.native`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.morph.native.md)
  and similar functions when called with
  `rglactions=list('no_vis'=TRUE)`) or a flat list of `coloredmesh`
  instances.

- background:

  string, background color passed to
  [`bg3d`](https://dmurdoch.github.io/rgl/dev/reference/bg.html).
  Defaults to "white".

- skip_all_na:

  logical, whether to skip (i.e., not render) meshes in the list that
  have the property 'render' set to FALSE. Defaults to TRUE.

- style, :

  a named list of style parameters or a string specifying an available
  style by name (e.g., 'shiny'). Defaults to 'default', the default
  style.

- rgloptions, :

  named list. Parameters passed to
  [`par3d`](https://dmurdoch.github.io/rgl/dev/reference/par3d.html).
  Defaults to the value returned by
  [`rglo`](https://dfsp-spirit.github.io/fsbrain/reference/rglo.md).

- ...:

  extra arguments passed to
  [`rglwidget`](https://dmurdoch.github.io/rgl/dev/reference/rglwidget.html).

## Value

an htmlwidget object from the rgl package, suitable for use with
[`rglwidgetOutput`](https://dmurdoch.github.io/rgl/dev/reference/shiny.html)
/
[`renderRglwidget`](https://dmurdoch.github.io/rgl/dev/reference/shiny.html)
in Shiny, or for direct embedding in RMarkdown HTML output.

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
[`vis.subject.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.annot.md),
[`vis.subject.label()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.label.md),
[`vis.subject.morph.native()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.morph.native.md),
[`vis.subject.morph.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.morph.standard.md),
[`vis.subject.pre()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.pre.md),
[`vis.symmetric.data.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.symmetric.data.on.subject.md),
[`vis.volume.on.surface()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.volume.on.surface.md),
[`vislayout.from.coloredmeshes()`](https://dfsp-spirit.github.io/fsbrain/reference/vislayout.from.coloredmeshes.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_optional_data();
   fsbrain::download_fsaverage(accept_freesurfer_license=TRUE);
   subjects_dir = fsbrain::get_optional_data_filepath("subjects_dir");

   # Get coloredmeshes without opening a window:
   cm = vis.subject.annot(subjects_dir, 'subject1', 'aparc', 'both',
         rglactions=list('no_vis'=TRUE));

   # Create an interactive WebGL widget:
   widget = vis.rglwidget(cm);

   # In RMarkdown, just print it:
   # widget

   # In Shiny, use rglwidgetOutput / renderRglwidget (see the shiny demo app).
} # }
```
