# fsbrain volume visualization

These are brain volume visualizations created using the [fsbrain R package](https://github.com/dfsp-spirit/fsbrain/).

## Lightbox views

A lightbox showing every 5th frame of a T1W volume, sagittal plane. A bounding box has been computed, so that empty space around the brain has been clipped automatically:

![Visvolume](https://github.com/dfsp-spirit/fsbrain_gallery/raw/master/volume/lightbox_axis1.png?raw=true "Lightbox view of a brain volume, rendered with fsbrain")

The same can be done for the [axial](https://github.com/dfsp-spirit/fsbrain_gallery/raw/master/volume/lightbox_axis2.png) and [coronal](https://github.com/dfsp-spirit/fsbrain_gallery/raw/master/volume/lightbox_axis3.png) planes.

You can also add activation data, p-values, [colors from a segmenation like aseg](https://github.com/dfsp-spirit/fsbrain_gallery/raw/master/volume/lightbox_aseg.png) or [aparc+aseg](https://github.com/dfsp-spirit/fsbrain_gallery/raw/master/volume/lightbox_aparc_plus_aseg.png), or whatever, as an overlay from a second volume file. Of course there is no need to plot all slices or every *n*th slice, you can select arbitrary combinations of slice indices to plot.

The function to achieve this is `volvis.lightbox`, or the more convenient replacement `volvis.lb` in newer fsbrain versions (>=0.5.0).

## Lightbox with surface contours

The `volvis.lb.with.surface()` function overlays cortical surface boundary contours onto 2D MRI lightbox slices — similar to FreeSurfer's `freeview` QA view. This is extremely useful for quality assessment (QA) of surface reconstructions against the underlying MRI volume.

The function supports overlaying **multiple surfaces at once**, each with its own color. For example, here the white surface is shown in red and the pial surface in yellow, on axial slices:

![Lightbox surface contours](https://github.com/dfsp-spirit/fsbrain_gallery/raw/master/volume/lightbox_surface_contours.png?raw=true "Lightbox view with white (red) and pial (yellow) surface contours overlaid on axial MRI slices, rendered with fsbrain")

Example code to produce this image:

```r
img <- volvis.lb.with.surface(subjects_dir, "subject1",
  volume = "brain",
  surface = c("white", "pial"),
  axis = 3L,
  surface_color = c("#FF0000", "#FFFF00"),
  surface_lwd = 1.5);

magick::image_write(img, "~/lightbox_surface_contours.png");
```

Different colors per hemisphere are also supported — see `?volvis.lb.with.surface` for details.

## Individual slice export with surface contours

The `volvis.slices.with.surface()` function is the single-slice export counterpart of `volvis.lb.with.surface()`. Instead of arranging slices in a lightbox grid, it exports each slice as a separate image file — ideal for browsing through slices one by one in an image viewer. It supports the same multi-surface overlay capabilities and uses the same underlying rendering engine.

The function supports two convenience options that are especially useful for QA: labelling the slice index directly on each image with `label_slices=TRUE`, and automatically skipping slices where no surface contour appears with `skip_empty=TRUE`.

![Slice with contours](https://github.com/dfsp-spirit/fsbrain_gallery/raw/master/volume/slice_with_contours.png?raw=true "Individual slice with white (red) and pial (yellow) surface contours, exported by fsbrain")

Example code to produce the image above (and all other slices):

```r
fsbrain::download_optional_data();
subjects_dir <- fsbrain::get_optional_data_filepath("subjects_dir");

volvis.slices.with.surface(subjects_dir, "subject1",
    volume = "brain",
    surface = c("white", "pial"),
    surface_color = c("#FF0000", "#FFFF00"),
    axis = 3L,
    slices = -3,
    output_dir = "~/fsbrain_qa_slices",
    output_prefix = "subject1",
    label_slices = TRUE,
    skip_empty = TRUE,
    silent = FALSE);
```

Output files are named following the pattern `<prefix>_axis<ax>_slice<idx>_<surfaces>_<hemis>.png`, e.g., `subject1_axis3_slice0136_white_pial_lh_rh.png`.

## Voxel-based 3D renderings

It is now possible to view volumes (the whole brain, segmentations, voxel-based activation values, or brain structures) in 3D in a voxel view. Here are two examples:

* [whole brain, aparc+aseg segmentation (24 MB animated GIF)](https://github.com/dfsp-spirit/fsbrain_gallery/blob/master/volume/animations/fsbrain_vox_aparc.gif)
* [brain ventricles, extracted from aseg segmentation (10 MB animated GIF)](https://github.com/dfsp-spirit/fsbrain_gallery/blob/master/volume/animations/fsbrain_vox_ventricles.gif)

Sorry for the glossy look and bad lighting in the images, this is fixed in the code already. Examples for how to generate this can be found in the [unit tests directory](../tests/).


## Animations

You can also render the volume slices into an animation. The result looks like this:

![Visvolumeanim](https://github.com/dfsp-spirit/fsbrain_gallery/raw/master/volume/animations/MRI_T1w_ax1.gif?raw=true "Animation in sagittal view, rendered with fsbrain")

The same can be done for the [axial](https://github.com/dfsp-spirit/fsbrain_gallery/raw/master/volume/animations/MRI_T1w_ax2.gif) and [coronal](https://github.com/dfsp-spirit/fsbrain_gallery/raw/master/volume/animations/MRI_T1w_ax3.gif) planes. Examples for how to generate this can be found in the [unit tests directory](../tests/).


Please [cite the fsbrain package](https://github.com/dfsp-spirit/fsbrain#citation) when [using](https://github.com/dfsp-spirit/fsbrain#license) these animations/videos.
