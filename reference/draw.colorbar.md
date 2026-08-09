# Draw colorbar into background of current plot.

Requires a rgl 3d visualisation to be open that already contains a
rendered object. Uses
[`bgplot3d`](https://dmurdoch.github.io/rgl/dev/reference/bgplot3d.html)
to add a colorbar in the background of the plot using
[`image.plot`](https://rdrr.io/pkg/fields/man/image.plot.html).
Experimental.

## Usage

``` r
draw.colorbar(coloredmeshes, horizontal = FALSE, ...)
```

## Arguments

- coloredmeshes:

  fs.coloredmesh as returned by the coloredmesh.from.\* functions.

- horizontal:

  logical, whether the colorbar should be drawn in horizontal
  orientation. Defaults to `TRUE`.

- ...:

  extra params passed to
  [`image.plot`](https://rdrr.io/pkg/fields/man/image.plot.html)

## Note

To adapt or change the colormap, you should use the 'makecmap_options'
parameter of the vis.\* function used to construct the coloredmeshes
(e.g.,
[`vis.subject.morph.native`](https://dfsp-spirit.github.io/fsbrain/reference/vis.subject.morph.native.md)).
