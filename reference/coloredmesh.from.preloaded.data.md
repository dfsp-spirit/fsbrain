# Generate coloredmesh from loaded data.

Generate coloredmesh from loaded data.

## Usage

``` r
coloredmesh.from.preloaded.data(
  fs_surface,
  morph_data = NULL,
  col = NULL,
  hemi = "lh",
  makecmap_options = mkco.seq()
)
```

## Arguments

- fs_surface:

  an fs.surface instance or a character string, which will be
  interpreted as the path to a file and loaded with
  [`freesurferformats::read.fs.surface`](https://rdrr.io/pkg/freesurferformats/man/read.fs.surface.html).

- morph_data:

  numerical vector, per-vertex data (typically morphometry) for the
  mesh. If given, takes precedence over 'col' parameter.

- col:

  vector of colors, typically hex color strings like '#FF00FF'. The
  per-vertex-colors for the mesh. Alternative to morph_data.

- hemi:

  character string, one of 'lh' or 'rh'. Metadata, the hemisphere. May
  be used by visualization functions to decide whether to draw the mesh
  in certain views.

- makecmap_options:

  named list of parameters to pass to
  [`makecmap`](https://rdrr.io/pkg/squash/man/makecmap.html). Must not
  include the unnamed first parameter, which is derived from 'measure'.

## Value

as fs.coloredmesh instance
