# Visualize a connectivity matrix as a connectome on a brain surface.

Draws the edges of a connectivity matrix as lines between region
centroids, the nodes as spheres, and (optionally) a semi-transparent
brain surface as context. This is the fsbrain equivalent of the
`plot_connectome` function of the Python package `yabplot`. It can be
used for any brain connectome, e.g., structural connectivity
(tractography), functional connectivity (correlation), or any other
square matrix that describes a relation between brain regions.

Note that this is a *visualization* function: it does not compute a
connectivity matrix, and it does not check whether the data is a valid
connectivity estimate.

## Usage

``` r
vis.connectome(
  connectivity_matrix,
  node_coords = NULL,
  node_names = NULL,
  subjects_dir = NULL,
  template_id = "fs_LR_32",
  atlas = NULL,
  context = list(surface = "midthickness", alpha = 0.08, color = "#B0B0B0"),
  edge_threshold = NULL,
  edge_threshold_quantile = 0.95,
  edge_absolute = TRUE,
  edge_negative = FALSE,
  edge_width = 1,
  edge_width_range = c(0.5, 3),
  edge_scale = c("weight", "none"),
  edge_color = NULL,
  edge_makecmap_options = modifyList(mkco.div(), list(symm = FALSE)),
  node_radius = 2,
  node_radius_range = NULL,
  node_scale = c("none", "strength"),
  node_color = NULL,
  node_makecmap_options = mkco.seq(),
  views = c("sd_lateral_lh", "sd_medial_lh", "sd_lateral_rh", "sd_medial_rh"),
  rgloptions = rglo(),
  rglactions = list(),
  style = "default",
  draw_colorbar = FALSE,
  silent = FALSE
)
```

## Arguments

- connectivity_matrix:

  numeric square matrix, the connectivity matrix. The rows and columns
  are the nodes (brain regions). The names of the rows/columns (or the
  parameter `node_names`) define the nodes, see the parameter
  `node_coords`. Alternatively, an edge list as a data.frame with the
  columns 'source', 'target' and 'weight'.

- node_coords:

  (n, 3) numeric matrix, the coordinates of the nodes (brain regions),
  in the same order as the matrix rows. If `NULL`, the nodes are looked
  up by name in the atlas given in parameter `atlas`, see the parameter
  `subjects_dir` and the function
  [`subject.region.centroids`](https://dfsp-spirit.github.io/fsbrain/reference/subject.region.centroids.md).
  The row names of the matrix (or the parameter `node_names`) must then
  match the atlas region names.

- node_names:

  vector of character strings, the node names. Only used if
  `node_coords` is given and the matrix has no row names. Defaults to
  NULL.

- subjects_dir:

  character string or NULL. The FreeSurfer `SUBJECTS_DIR`, see
  [`subject.region.centroids`](https://dfsp-spirit.github.io/fsbrain/reference/subject.region.centroids.md).
  Defaults to NULL, which searches the standard locations for the
  template subject `template_id`.

- template_id:

  character string, the identifier of the template (or subject) whose
  atlas and surface define the nodes and the context surface. Defaults
  to 'fs_LR_32'. Ignored if `node_coords` is given *and* `context` is
  NULL.

- atlas:

  character string or NULL, the atlas whose regions are the nodes, e.g.,
  'schaefer400'. Not required if `node_coords` is given.

- context:

  named list or NULL, the semi-transparent brain surface drawn behind
  the connectome. The list can have the entries: 'surface' (character
  string, the surface to use, defaults to 'midthickness'), 'alpha'
  (numeric, the transparency of the surface, defaults to 0.08), and
  'color' (character string, the color of the surface, defaults to
  '#B0B0B0'). Set to NULL to draw the connectome without any brain
  surface, which is much faster and gives a 'spring layout' style figure
  (the camera then frames the nodes).

- edge_threshold:

  numeric or NULL, an absolute threshold on the edge weight. Edges with
  a lower magnitude are not drawn. Defaults to NULL (no absolute
  threshold).

- edge_threshold_quantile:

  numeric or NULL, a quantile (in the range 0 to 1) of the edge
  magnitudes. Only edges with a magnitude above that quantile are drawn,
  i.e., the quantile selects the strongest edges. Defaults to 0.95,
  i.e., the strongest 5 percent of the edges are drawn. Set to NULL to
  use all edges.

- edge_absolute:

  logical, whether the magnitude of the edge weights is used for
  thresholding and for the line widths. Defaults to TRUE, i.e., a strong
  negative edge is drawn like a strong positive one (but see the
  parameter `edge_negative`).

- edge_negative:

  logical, whether to draw the negative edges instead of the positive
  ones. Defaults to FALSE, i.e., only positive edges are considered. To
  show both the positive and the negative edges of a signed matrix in
  one figure, call this function twice and combine the results (the
  second call can be rendered on top of the first).

- edge_width:

  positive number, the line width in pixels. Defaults to 1.0 (a thin
  line, which is what hardware line rendering supports best).

- edge_width_range:

  numeric vector of length 2 or NULL, the range of the line widths used
  when `edge_scale` is 'weight'. Defaults to c(0.5, 3.0).

- edge_scale:

  character string, one of 'weight' (the line width encodes the edge
  weight) or 'none' (all edges have the width `edge_width`). Defaults to
  'weight'.

- edge_color:

  vector of hex color strings or NULL. A single color for all edges, or
  one color per edge. If NULL (the default), the edges are colored by
  their weight, using the colormap defined by `edge_makecmap_options`.

- edge_makecmap_options:

  named list of parameters to pass to
  [`makecmap`](https://rdrr.io/pkg/squash/man/makecmap.html), see
  [`mkco.div`](https://dfsp-spirit.github.io/fsbrain/reference/mkco.div.md).
  Used to color the edges by weight and to draw the colorbar. Defaults
  to a diverging colormap which spans the weights of the drawn edges
  (note the 'symm' setting: only one sign of edges is drawn per call, so
  a symmetric range would waste half of the colormap).

- node_radius:

  positive number, the radius of the node spheres, in the units of the
  surface coordinates (millimeters). Defaults to 2.0.

- node_radius_range:

  numeric vector of length 2 or NULL, the range of the node radii used
  when `node_scale` is 'strength'. Defaults to NULL, which uses 0.5 and
  1.5 times `node_radius`.

- node_scale:

  character string, one of 'none' (all nodes have the radius
  `node_radius`) or 'strength' (the node radius encodes the node
  strength, i.e., the sum of the absolute weights of all its edges).
  Defaults to 'none'.

- node_color:

  vector of hex color strings or NULL. A single color for all nodes, or
  one color per node. If NULL (the default), the nodes are colored by
  their strength (see `node_scale`), using the colormap defined by
  `node_makecmap_options`.

- node_makecmap_options:

  named list of parameters to pass to
  [`makecmap`](https://rdrr.io/pkg/squash/man/makecmap.html), see
  [`mkco.seq`](https://dfsp-spirit.github.io/fsbrain/reference/mkco.seq.md).
  Used to color the nodes by strength and to draw the colorbar. Defaults
  to
  [`mkco.seq()`](https://dfsp-spirit.github.io/fsbrain/reference/mkco.seq.md).

- views:

  vector of character strings, the views to render, see
  [`get.view.angle.names`](https://dfsp-spirit.github.io/fsbrain/reference/get.view.angle.names.md).
  Defaults to the 4 standard views.

- rgloptions:

  option list passed to
  [`par3d`](https://dmurdoch.github.io/rgl/dev/reference/par3d.html),
  see [`rglo`](https://dfsp-spirit.github.io/fsbrain/reference/rglo.md).

- rglactions:

  named list. A list in which the names are from a set of pre-defined
  actions, see
  [`rglactions`](https://dfsp-spirit.github.io/fsbrain/reference/rglactions.md).

- style:

  a rendering style for the surface meshes, see
  [`get.rglstyle`](https://dfsp-spirit.github.io/fsbrain/reference/get.rglstyle.md).
  Note that the style of the context surface and of the nodes is not
  affected by this, only the style requested for the *edges* (which is
  of little relevance, as lines are drawn unlit by default).

- draw_colorbar:

  logical or one of the character strings 'vertical' or 'horizontal',
  whether to draw a colorbar for the *edges*. Defaults to FALSE. Note
  that a colorbar is only drawn for the edges, not for the nodes: if you
  want to draw a colorbar for the node strengths, use
  [`coloredmesh.plot.colorbar.separate`](https://dfsp-spirit.github.io/fsbrain/reference/coloredmesh.plot.colorbar.separate.md)
  on the nodes (the entry 'nodes' of the return value). Also note that
  the headless scimesh renderer backend does not support colorbars in
  this function, use
  [`export`](https://dfsp-spirit.github.io/fsbrain/reference/export.md)
  instead (see the return value).

- silent:

  logical, whether to suppress the status messages. Defaults to FALSE.

## Value

named list of renderables, invisibly: the entries 'edges' (an
`fs.coloredpaths` instance), 'nodes' (an `fs.coloredmesh` instance) and
'context_lh'/'context_rh' (the context surface, if requested). This list
can be passed to
[`export`](https://dfsp-spirit.github.io/fsbrain/reference/export.md) to
create a publication quality figure with a colorbar, which is also the
recommended way to use the scimesh renderer backend.

## See also

Other connectome functions:
[`subject.region.centroids()`](https://dfsp-spirit.github.io/fsbrain/reference/subject.region.centroids.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  # Use the fs_LR_32 template (see subject.region.centroids for the download commands).
  centroids = subject.region.centroids(subjects_dir = NULL, subject_id = "fs_LR_32",
    atlas = "schaefer400", surface = "midthickness");
  set.seed(42);
  mat = matrix(runif(nrow(centroids)^2), nrow = nrow(centroids),
    dimnames = list(rownames(centroids), rownames(centroids)));
  mat = (mat + t(mat)) / 2;
  diag(mat) = 0;

  # Render the connectome in 3 views:
  cm = vis.connectome(mat, atlas = "schaefer400",
    views = c("sd_lateral_lh", "sd_dorsal", "sd_caudal"));

  # Or create a publication quality image with a colorbar for the edges:
  export(cm, draw_colorbar = "horizontal",
    output_img = "connectome.png", colorbar_legend = "Edge weight");
} # }
```
