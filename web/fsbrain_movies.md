# fsbrain animations

These are animations created using the [fsbrain R package](https://github.com/dfsp-spirit/fsbrain/), with a lot of help from [rgl](https://CRAN.R-project.org/package=rgl) and some polish from [ImageMagick](https://imagemagick.org).

![Vissurfaces](./fsbrain_morphnat_curv_web.gif?raw=true "Rotating brain mesh with mean curvature plotted onto it, rendered with fsbrain")

**Animation 1:** Rotating visualization of mean curvature plotted onto the white surface of a subject (native space).


# More descriptors and brain parcellations (GIF format)

Have some bandwidth to spare? Try some of these large versions for different surface-based morphometry measures in native space:


* [Mean curvature (12 MB)](./fsbrain_morphnat_curv.gif)
* [Jacobian white (9.3 MB)](./fsbrain_morphnat_jacobian_white.gif)
* [Sulcal depth (9.0 MB)](./fsbrain_morphnat_sulc.gif)
* [Mean Geodesic distance (11 MB)](./fsbrain_morphnat_geodesic_white.gif)


Here are some brain surface parcellations:

* [The Desikan-Killiany Atlas (5.9 MB)](./fsbrain_atlas_aparc.gif)
* [The Destrieux atlas (6.8 MB)](./fsbrain_atlas_a2009s.gif)


Here is some standard space data, smoothed at FWHM 10 and visualized on the *fsaverage* template:

* [White surface area (10 MB)](./fsbrain_morphstd_area_fwhm10.gif)
* [Mean curvature (12 MB)](./fsbrain_morphstd_curv_fwhm10.gif)
* [Local Gyrification Index (10 MB)](./fsbrain_morphstd_pial_lgi_fwhm10.gif)
* [Sulcal depth (11 MB)](./fsbrain_morphstd_sulc_fwhm10.gif)
* [Cortical thickness (11 MB)](./fsbrain_morphstd_thickness_fwhm10.gif)
* [Cortical volume (11 MB)](./fsbrain_morphstd_volume_fwhm10.gif)


# Encoded videos

If you prefer videos over gif animations, you can check out the [fsbrain channel on vimeo](https://vimeo.com/channels/fsbrain). These videos were created from the animation frames using [ffmpeg](https://www.ffmpeg.org/).


Please [cite the fsbrain package](https://github.com/dfsp-spirit/fsbrain#citation) when using these animations/videos.
