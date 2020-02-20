# fsbrain volume visualization

These are brain volume visualizations created using the [fsbrain R package](https://github.com/dfsp-spirit/fsbrain/).

## Lightbox views

A lightbox showing every 5th frame of a T1W volume, sagittal plane. A bounding box has been computed, so that empty space around the brain has been clipped automatically:

![Visvolume](https://github.com/dfsp-spirit/fsbrain_gallery/raw/master/volume/lightbox_axis1.png?raw=true "Lightbox view of a brain volume, rendered with fsbrain")

The same can be done for the [axial](https://github.com/dfsp-spirit/fsbrain_gallery/raw/master/volume/lightbox_axis2.png) and [coronal](https://github.com/dfsp-spirit/fsbrain_gallery/raw/master/volume/lightbox_axis3.png) planes.

You can also add activation data, p-values, [colors from a segmenation like aseg](https://github.com/dfsp-spirit/fsbrain_gallery/raw/master/volume/lightbox_aseg.png) or [aparc+aseg](https://github.com/dfsp-spirit/fsbrain_gallery/raw/master/volume/lightbox_aparc_plus_aseg.png), or whatever, as an overlay from a second volume file. Of course there is no need to plot all slices or every *n*th slice, you can select arbitrary combinations of slice indices to plot.


## Voxel-based 3D renderings

It is now possible to view volumes (the whole brain, segmentations, voxel-based activation values, or brain structures) in 3D in a voxel view. Here are two examples:

* [whole brain, aparc+aseg segmentation (24 MB animated GIF)](https://github.com/dfsp-spirit/fsbrain_gallery/blob/master/volume/animations/fsbrain_vox_aparc.gif)
* [brain ventricles, extracted from aseg segmentation (10 MB animated GIF)](https://github.com/dfsp-spirit/fsbrain_gallery/blob/master/volume/animations/fsbrain_vox_ventricles.gif)

Sorry for the glossy look and bad lighting in the images, this is fixed in the code already.


## Animations

You can also render the volume slices into an animation. The result looks like this:

![Visvolumeanim](https://github.com/dfsp-spirit/fsbrain_gallery/raw/master/volume/animations/MRI_T1w_ax1.gif?raw=true "Animation in sagittal view, rendered with fsbrain")

The same can be done for the [axial](https://github.com/dfsp-spirit/fsbrain_gallery/raw/master/volume/animations/MRI_T1w_ax2.gif) and [coronal](https://github.com/dfsp-spirit/fsbrain_gallery/raw/master/volume/animations/MRI_T1w_ax3.gif) planes.


Please [cite the fsbrain package](https://github.com/dfsp-spirit/fsbrain#citation) when [using](https://github.com/dfsp-spirit/fsbrain#license) these animations/videos.
