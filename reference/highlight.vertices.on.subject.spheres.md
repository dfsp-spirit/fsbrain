# Highlight vertices given by index on a subject's meshes by coloring faces.

Highlight vertices given by index on a subject's meshes by coloring
faces.

## Usage

``` r
highlight.vertices.on.subject.spheres(
  subjects_dir,
  vis_subject_id,
  vertices,
  surface = "white",
  patch_size = 25,
  show_patch = TRUE,
  style = "glass2",
  export_img = NULL,
  sphere_colors = c("#FF0000"),
  sphere_radius = 3,
  ...
)
```

## Arguments

- subjects_dir:

  string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the
  data for all your subjects, each in a subdir named after the subject
  identifier.

- vis_subject_id:

  string. The subject identifier from which to obtain the surface for
  data visualization. Example: 'fsaverage'.

- vertices:

  positive integer vector, the vertex indices over both hemispheres.
  Alternative to using verts_lh and verts_rh parameters, only one of
  them must be used at once.

- surface:

  string. The display surface. E.g., "white", "pial", or "inflated".
  Defaults to "white".

- patch_size:

  double, geodesic radius in which to draw a patch on the mesh around
  the verts. Pass `NULL` to disable.

- show_patch:

  logical (or a vector with one logical value per entry in 'vertices'),
  whether to show colored geodesic patches at the highlighted vertices.

- style:

  character string or rgl rendering style, see
  [`get.rglstyle`](https://dfsp-spirit.github.io/fsbrain/reference/get.rglstyle.md).

- export_img:

  character string, the path to the output image if you want to export a
  high-quality image, NULL if you want live visualization instead.

- sphere_colors:

  the sphere colors like '#FF0000', can be a single one for all or one
  per sphere

- sphere_radius:

  double, a single radius for all spheres

- ...:

  extra parameters passed on to
  [`vis.data.on.subject`](https://dfsp-spirit.github.io/fsbrain/reference/vis.data.on.subject.md).
  Use this to set a custom colormap etc.

## Value

list of coloredmeshes. The coloredmeshes used for the visualization. If
export_img is set, the export return value is returned instead.

## Note

If no patches are visualized, the color used for the brain can be set
with `options("fsbrain.brain_na_color"="#FF0000")`.

## See also

Other visualization functions:
[`highlight.vertices.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/highlight.vertices.on.subject.md),
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
[`vis.volume.on.surface()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.volume.on.surface.md),
[`vislayout.from.coloredmeshes()`](https://dfsp-spirit.github.io/fsbrain/reference/vislayout.from.coloredmeshes.md)

Other surface visualization functions:
[`highlight.vertices.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/highlight.vertices.on.subject.md),
[`vis.color.on.subject()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.color.on.subject.md)

## Examples

``` r
if (FALSE) { # \dontrun{
   fsbrain::download_fsaverage(T);
   subjects_dir = fsaverage.path();
   mkco = list('colFn'=viridis::viridis, 'n'=300);
   # Ex.1: highlight with patches and custom colormap:
   highlight.vertices.on.subject.spheres(subjects_dir, 'fsaverage',
     vertices=c(300, 5000, 100000), makecmap_options = mkco);
   # Ex.2: show patches on some (red) vertices, not on blue ones:
   highlight.vertices.on.subject.spheres(subjects_dir, 'fsaverage',
     vertices=c(300, 5000, 100000, 300000), show_patch = c(T,F,T,F),
     sphere_colors = c("red", "blue", "red", "blue"));
} # }
```
