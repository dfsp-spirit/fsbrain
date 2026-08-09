# Plot coloredmeshes for a group of subjects.

Plot coloredmeshes for a group of subjects into a single image.

## Usage

``` r
vis.group.coloredmeshes(
  coloredmeshes,
  view_angles = "sd_dorsal",
  output_img = "fsbrain_group_annot.png",
  num_per_row = 5L,
  captions = NULL,
  background_color = "white"
)
```

## Arguments

- coloredmeshes:

  a list of coloredmeshes lists, each entry in the outer list contains
  the hemilist of coloredmeshes (lefgt and right hemisphere mesh) for
  one subject.

- view_angles:

  see
  [`get.view.angle.names`](https://dfsp-spirit.github.io/fsbrain/reference/get.view.angle.names.md).

- output_img:

  character string, the file path for the output image. Should end with
  '.png'.

- num_per_row:

  positive integer, the number of tiles per row.

- captions:

  optional vector of character strings, the short text annotations for
  the individual tiles. Typically used to plot the subject identifier.

- background_color:

  color for image background (transparency is not supported).

## Value

named list, see the return value of
[`arrange.brainview.images.grid`](https://dfsp-spirit.github.io/fsbrain/reference/arrange.brainview.images.grid.md)
for details.

## Note

This is a mid-level function, end users may want to call high-level
functions like
[`vis.group.annot`](https://dfsp-spirit.github.io/fsbrain/reference/vis.group.annot.md)
instead.

## See also

Other group visualization functions:
[`vis.data.on.group.native()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.data.on.group.native.md),
[`vis.data.on.group.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.data.on.group.standard.md),
[`vis.group.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.group.annot.md),
[`vis.group.morph.native()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.group.morph.native.md),
[`vis.group.morph.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.group.morph.standard.md)
