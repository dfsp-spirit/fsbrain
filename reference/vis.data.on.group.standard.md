# Visualize standard space data for a group on template.

Plot standard space data for a group of subjects onto a template brain
and combine the tiles into a single large image.

## Usage

``` r
vis.data.on.group.standard(
  subjects_dir,
  vis_subject_id,
  morph_data_both,
  captions = NULL,
  view_angles = "sd_dorsal",
  output_img = "fsbrain_group_morph.png",
  num_per_row = 5L,
  rglactions = list(no_vis = TRUE),
  ...
)
```

## Arguments

- subjects_dir:

  character string, the path to the SUBJECTS_DIR containing the template
  subject

- vis_subject_id:

  character string, the template subject name. A typical choice is
  'fsaverage'.

- morph_data_both:

  named list of numerical vectors, 4D array or dataframe, the morph data
  for both hemispheres of all subjects. Can be loaded with
  [`group.morph.standard`](https://dfsp-spirit.github.io/fsbrain/reference/group.morph.standard.md)
  or
  [`group.morph.standard.sf`](https://dfsp-spirit.github.io/fsbrain/reference/group.morph.standard.sf.md).

- captions:

  optional vector of character strings, the short text annotations for
  the individual tiles. Typically used to plot the subject identifier.

- view_angles:

  see
  [`get.view.angle.names`](https://dfsp-spirit.github.io/fsbrain/reference/get.view.angle.names.md).

- output_img:

  character string, the file path for the output image. Should end with
  '.png'.

- num_per_row:

  positive integer, the number of tiles per row.

- rglactions:

  named list. A list in which the names are from a set of pre-defined
  actions. The values can be used to specify parameters for the action.
  The following example clips outliers in the data before plotting and
  writes a screenshot in PNG format:
  `rglactions = list("snapshot_png"="~/fsbrain.png", "clip_data"=c(0.05, 0.95))`.
  See
  [`rglactions`](https://dfsp-spirit.github.io/fsbrain/reference/rglactions.md).

- ...:

  extra parameters passed to the subject level visualization function.
  Not all may make sense in this context. Example: `surface='pial'`.

## Value

named list, see the return value of
[`arrange.brainview.images.grid`](https://dfsp-spirit.github.io/fsbrain/reference/arrange.brainview.images.grid.md)
for details.

## Note

The subject data are plotted row-wise, in the order in which they appear
in the 'morph_data_both' parameter.

You can force an identical plot range for all subjects, so that one
color represents identical values across subjects, via
'makecmap_options'. E.g., for the ... parameter, pass
`makecmap_options=list('colFn'=viridis::viridis, 'range'=c(0, 4)))`.

## See also

Other group visualization functions:
[`vis.data.on.group.native()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.data.on.group.native.md),
[`vis.group.annot()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.group.annot.md),
[`vis.group.coloredmeshes()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.group.coloredmeshes.md),
[`vis.group.morph.native()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.group.morph.native.md),
[`vis.group.morph.standard()`](https://dfsp-spirit.github.io/fsbrain/reference/vis.group.morph.standard.md)
