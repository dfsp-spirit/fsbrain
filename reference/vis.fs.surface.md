# Visualize fs.surface mesh

Render a mesh. All mesh formats supported by the *freesurferformats*
package are supported, including OFF, PLY, OBJ, STL, and many more.

## Usage

``` r
vis.fs.surface(
  fs_surface,
  col = "white",
  per_vertex_data = NULL,
  hemi = "lh",
  makecmap_options = mkco.seq(),
  ...
)
```

## Arguments

- fs_surface:

  an fs.surface instance, as returned by function like
  [`subject.surface`](https://dfsp-spirit.github.io/fsbrain/reference/subject.surface.md)
  or
  [`read.fs.surface`](https://rdrr.io/pkg/freesurferformats/man/read.fs.surface.html).
  If a character string, it is assumed to be the full path of a surface
  file, and the respective file is loaded with
  [`read.fs.surface`](https://rdrr.io/pkg/freesurferformats/man/read.fs.surface.html).
  If parameter 'hemi' is 'both', this must be a hemilist. A single
  `rgl::tmesh` is also fine.

- col:

  vector of colors, the per-vertex-colors. Defaults to white. Must be a
  single color or one color per vertex. If parameter 'hemi' is 'both',
  this must be a hemilist.

- per_vertex_data:

  numerical vector, per-vertex data. If given, takes precedence over
  'col'. Used to color the mesh using the colormap options in parameter
  'makecmap_options'. If a character string, it is assumed to be the
  full path of a morphometry data file, and the respective file is
  loaded with
  [`read.fs.morph`](https://rdrr.io/pkg/freesurferformats/man/read.fs.morph.html).
  If parameter 'hemi' is 'both', this must be a hemilist.

- hemi:

  character string, one of 'lh' or 'rh'. This may be used by
  visualization functions to decide whether or not to show this mesh in
  a certain view.

- makecmap_options:

  named list of parameters to pass to
  [`makecmap`](https://rdrr.io/pkg/squash/man/makecmap.html). Must not
  include the unnamed first parameter, which is derived from 'measure'.
  Should include at least a colormap function as name 'colFn'.

- ...:

  extra parameters to pass to
  [`vis.coloredmeshes`](https://dfsp-spirit.github.io/fsbrain/reference/vis.coloredmeshes.md).

## Value

see
[`vis.coloredmeshes`](https://dfsp-spirit.github.io/fsbrain/reference/vis.coloredmeshes.md)

## Note

This function can be used to visualize arbitrary triangular meshes in R.
Despite its name, it is not limited to brain surface meshes.
