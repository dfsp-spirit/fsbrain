# Compute the shared colormap of the cluster shells.

All shells of both signs share one colormap, so that the color of a
shell is exactly the color that the colorbar shows for its iso-level. A
symmetric range which covers all iso-levels is used unless the user
requested a range via the 'range' entry of `makecmap_options`.

## Usage

``` r
volume.clusters.colormap(makecmap_options, levels, max_abs = max(abs(levels)))
```

## Arguments

- makecmap_options:

  named list, the colormap options, see
  [`makecmap`](https://rdrr.io/pkg/squash/man/makecmap.html).

- levels:

  numerical vector of iso-levels.

- max_abs:

  single positive number, the absolute value of the most extreme value
  that the colormap has to cover, i.e., half of the range of the
  colormap. Defaults to the highest absolute iso-level.

## Value

named list with the entries 'map' (the `squash` colormap), 'colors' (a
color per entry of `levels`), 'range' (the range of the colormap) and
'options' (`makecmap_options`, including the range, for the colorbar
metadata of the meshes).
