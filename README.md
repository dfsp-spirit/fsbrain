# fsbrain

<!-- badges: start -->
[![DOI](https://zenodo.org/badge/209085379.svg)](https://zenodo.org/doi/10.5281/zenodo.3559816)
[![HiRSE Code Promo Badge](https://img.shields.io/badge/Promo-8db427?label=HiRSE&labelColor=005aa0&)](https://go.fzj.de/CodePromo)


[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/dfsp-spirit/fsbrain?branch=master&svg=true)](https://ci.appveyor.com/project/dfsp-spirit/fsbrain) AppVeyor CI under Windows
<!-- badges: end -->


An R package for structural neuroimaging. Provides high-level functions to access (read and write) and visualize surface-based brain morphometry data (e.g. cortical thickness) for individual subjects and groups.

![Fig1](https://github.com/dfsp-spirit/fsbrain_gallery/blob/master/surface/fsbrain_sulcal_depth_cbar_web.jpg?raw=true "Sulcal depth visualization, created with fsbrain")

**Fig.1**: *Visualization of sulcal depth for a subject in FreeSurfer standard space (fsaverage). See the [source code to reproduce this image](https://htmlpreview.github.io/?https://github.com/dfsp-spirit/fsbrain/blob/develop/web/Rmd_web_examples/examples_export.html) in an R notebook.*


![Fig2](https://github.com/dfsp-spirit/fsbrain_gallery/blob/master/surface/fsbrain_clusters_cbar_web.jpg?raw=true "Statistical results visualization, created with fsbrain")

**Fig.2**: *Visualization of statistical results (clusters) in FreeSurfer standard space (fsaverage) with a diverging colormap. See the [source code to reproduce this image](https://htmlpreview.github.io/?https://github.com/dfsp-spirit/fsbrain/blob/develop/web/Rmd_web_examples/examples_export.html) in an R notebook.*

[About](#about) | [Installation](#installation) | [Documentation](#documentation) | [Unit tests](#unit-tests-and-continuous-integration) | [License](#license) | [Citation](#citation) | [Visualization examples](#visualization-examples) | [Contributing](#contributing)


## About

The *fsbrain* R package provides a well-tested and consistent interface to neuroimaging data in [R](https://www.r-project.org/). It supports reading, writing, and visualizing various kinds of raw data and statistical results on brain surfaces and volumes. While the package provides a very convenient interface for working with data arranged in the standard [FreeSurfer](https://freesurfer.net/) directory structure (SUBJECTS_DIR), *fsbrain* is not limited to this layout or FreeSurfer file formats. You can load brain meshes, volumes, and data from a range of other neuroimaging software packages and visualize them.

The plots produced by *fsbrain* can be integrated into R notebooks or written to high-quality bitmap image files, ready for publication. By default, *fsbrain* uses the [rgl](https://CRAN.R-project.org/package=rgl) package for rendering, which provides fast, hardware-accelerated 3D graphics based on OpenGL. As an alternative, *fsbrain* also supports the [scimesh](https://CRAN.R-project.org/package=scimesh) software renderer — a headless, GPU-free C++ renderer that produces identical static images without requiring X11, OpenGL, or a GPU. This is ideal for headless servers, HPC clusters, or macOS systems where XQuartz is broken.


## News
* 2026-08-23: New documentation for the scimesh rendering backend: a new vignette (`vignette("fsbrain_with_scimesh")`) and an [online notebook with pre-rendered figures](https://htmlpreview.github.io/?https://github.com/dfsp-spirit/fsbrain/blob/develop/web/Rmd_web_examples/fsbrain_with_scimesh.html) that show how to configure fsbrain for headless, GPU-free static image export and how to use the `export()` API, including region- and vertex-based results and a workflow with manually loaded meshes.
* 2026-07-14: New alternative rendering backend via [scimesh](https://CRAN.R-project.org/package=scimesh). Switchable with `options(fsbrain.renderer_backend = "scimesh")`. Enables publication-quality static images without X11/OpenGL/GPU — great for macOS Tahoe/Sonoma, HPC clusters, and headless servers.
* 2026-08-20: We have some new online documentation: example notebooks demonstrating 2 typical workflows of fMRI result visualization with fsbrain. [View them online here](https://dfsp-spirit.github.io/fsbrain_fMRI_vis_workflows/).
* 2026-07-09: New fsbrain version 0.6.1 released. Adds automatic fallback for plot export on recent macOS versions (Tahoe, Sonoma) where X11/XQuartz is broken. You can now export publication-ready plots with colorbars even without a working X11 display. See [README_MACOS_TAHOE.md](./README_MACOS_TAHOE.md) for details.
* 2026-07-08: New fsbrain version 0.6.0 released on CRAN, see the [CHANGES](./CHANGES).
* 2025-09-09: New fsbrain version 0.5.6 released on CRAN, see the [CHANGES](./CHANGES).
* 2024-02-03: New fsbrain version 0.5.5 released on CRAN, see the [CHANGES](./CHANGES).
* 2023-06-26: New fsbrain version 0.5.4 released on CRAN, see the [CHANGES](./CHANGES).
* 2022-12-22: We are looking for help! If you have a Mac and are interested in helping fsbrain development, please [contact us by email](http://rcmd.org/ts/#contact) or reply to [#46](https://github.com/dfsp-spirit/fsbrain/issues/46) here on GitHub!
* 2022-02-13: New fsbrain version 0.5.3 released on CRAN, see the [CHANGES](./CHANGES).
* 2021-11-11: New fsbrain version 0.5.1 released on CRAN, see the [CHANGES](./CHANGES).
* 2021-09-16: New fsbrain version 0.5.0 released on CRAN, see the [CHANGES](./CHANGES).
* 2021-05-12: New fsbrain version 0.4.3 released on CRAN, see the [CHANGES](./CHANGES).
* 2021-03-28: New fsbrain version 0.4.2 released on CRAN, see the [CHANGES](./CHANGES).
* 2020-09-20: The preprint of our paper [T. Schaefer, C. Ecker: fsbrain: an R package for the visualization of structural neuroimaging data](https://doi.org/10.1101/2020.09.18.302935)' is now available on biorxiv.

You can find all releases in the [releases section](https://github.com/dfsp-spirit/fsbrain/releases).


## Installation


### Recommended: install the stable fsbrain version from CRAN

You can find the [fsbrain package on CRAN](https://cran.r-project.org/package=fsbrain), so all you need to do is:

```r
install.packages("fsbrain");
```

In case something goes wrong, don't worry. Just install the missing [system dependencies](#system-dependencies) and retry.


### Optional: scimesh rendering backend for headless environments

For headless environments (HPC clusters, servers, CI runners) or macOS systems where XQuartz is broken, you can use the [scimesh](https://CRAN.R-project.org/package=scimesh) software renderer instead of rgl. It produces identical publication-ready static images without X11, OpenGL, XQuartz, or a GPU.

```r
install.packages("scimesh");
```

To activate the scimesh backend for the current R session:

```r
options(fsbrain.renderer_backend = "scimesh");
```

Static image export (`vislayout.from.coloredmeshes()` and `export()`) now renders with scimesh. Interactive `views` (e.g., `views = "si"`, `"sr"`, `"t4"`, `"t9"`) still use rgl. Switch back at any time:

```r
options(fsbrain.renderer_backend = "rgl");
```

Set the output image resolution (default 1920x1080) with the `fsbrain.scimesh.output_dims` option, e.g. `options(fsbrain.scimesh.output_dims = c(1600, 900))`.

**What scimesh supports**: All static image export (single views, multi-view layouts, colorbars), all rendering styles. **What it does not**: Interactive 3D windows, real-time rotation, browser-based widgets (`vis.rglwidget`), animated GIFs — these remain available through the default rgl backend. In practice, most users only need static images for presentations and publications, for which scimesh works perfectly.



### System dependencies

A *system dependency* is a **non-R** software that is needed for the installation of a package. System dependencies cannot be installed automatically using the R package system, so you need to install them manually or using the package manager of your operating system.

The *fsbrain* package itself does not have any system dependencies, however, it uses *rgl* for rendering. You can check the *SystemRequirements* section on the [rgl page at CRAN](https://CRAN.R-project.org/package=rgl) for the full list of rgl dependencies or read on. To get GIFTI format support, you will also need `libxml2-dev`.

**Note**: If you use the [scimesh](https://CRAN.R-project.org/package=scimesh) backend, none of the rgl system dependencies are required — scimesh is a pure C++ software renderer with no external library dependencies beyond a C++ compiler.

To install the system dependencies for *rgl* and *xml2*:

#### Linux System dependencies (or: building from source)

R packages are compiled from source by default under Linux, so you need some development libraries. Before installing *fsbrain*, run the following command in your system shell (not in R):

* for deb-based Linux distributions (Debian, Ubuntu, ...):
```shell
sudo apt-get install libmagick++-dev libx11-dev libgl1-mesa-dev libglu1-mesa-dev mesa-common-dev libfreetype6-dev libxml2-dev libssh-dev libcurl4-openssl-dev gfortran libblas-dev liblapack-dev libgfortran5
```

Note: For older Ubuntu versions, you may have to replace ```libgfortan5``` in the command above with ```libgfortan4```.


* for rpm-based Linux distributions (Fedora, CentOS, RHEL, ...):
```shell
sudo yum install ImageMagick-c++-devel libX11-devel mesa-libGLU-devel freetype-devel libxml2-devel
```

If you want to compile the package under any other operating system, you will need the libraries as well, of course.

#### MacOS System dependencies

Recent macOS versions do not ship with an X11 environment. If you want to use the default rgl backend for interactive viewing, you will need to install [XQuartz](https://www.xquartz.org/). If you want to create GIF movies, make sure you have imagemagick installed (easiest via [homebrew](https://brew.sh/): `brew install imagemagick@6`).

**Recommended for static image export**: Use the [scimesh backend](#optional-scimesh-rendering-backend-for-headless-environments), which requires neither X11 nor XQuartz and produces identical publication-ready images.

Note that X11 is not needed for rendering, but only for opening interactive windows. If you only need publication-quality static images (which is the typical use case), the scimesh backend or the browser-based `rglwidget` are better options.

#### Known issue: Visualization problems on recent macOS versions with rgl

If fsbrain does not open visualization windows or produces blank plots on recent macOS versions (Tahoe 26.x or Sonoma 14.x), see [README_MACOS_TAHOE.md](./README_MACOS_TAHOE.md) for details. The new topional scimesh renderer solves most of these issues.


#### Windows Installation Hints

Under Windows 10, it seems that you will need to install these two packages manually via the `install.packages` command: `shiny` and `manipulateWidget`.


### Installation via Docker

There are Docker images for fsbrain available on Dockerhub, see the [fsbrain Dockerhub repo](https://hub.docker.com/r/dfspspirit/fsbrain).


## Documentation

The documentation can be accessed from within an R session after you have loaded the *fsbrain* package:

* There are several online R Markdown notebooks (like Jupyter Notebook in Python) that show various example plots in combination with the code used to produce them:
  * [basic fsbrain example notebook](https://dfsp-spirit.github.io/fsbrain/notebooks/examples.html): Live visualization of subject data
  * [advanced fsbrain example notebook](https://dfsp-spirit.github.io/fsbrain/notebooks/examples_adv.html): Plotting group data
  * [export API fsbrain example notebook](https://dfsp-spirit.github.io/fsbrain/notebooks/examples_export.html): Exporting publication-ready plots
  * [fsbrain with the scimesh rendering backend](https://dfsp-spirit.github.io/fsbrain/notebooks/fsbrain_with_scimesh.html): Headless, GPU-free static image export


* Detailed vignettes with explanations and examples for the functions of the package is included, run `browseVignettes("fsbrain")` to see the vignettes. You can also open the vignettes online — the GitHub Pages versions always reflect the latest development state (no new CRAN release required); the CRAN versions are updated on releases:
  * How to load and visualize surface-based neuroimaging data: `vignette("fsbrain")` or: [read online](https://dfsp-spirit.github.io/fsbrain/articles/fsbrain.html) (also on [CRAN](https://cran.r-project.org/web/packages/fsbrain/vignettes/fsbrain.html))
  * How to load and visualize volume-based neuroimaging data: `vignette("fsbrain_vol")` or: [read online](https://dfsp-spirit.github.io/fsbrain/articles/fsbrain_vol.html) (also on [CRAN](https://cran.r-project.org/web/packages/fsbrain/vignettes/fsbrain_vol.html))
  * The fsbrain FAQ: `vignette("fsbrain_faq")` or: [read online](https://dfsp-spirit.github.io/fsbrain/articles/fsbrain_faq.html) (also on [CRAN](https://cran.r-project.org/web/packages/fsbrain/vignettes/fsbrain_faq.html))
  * How to use the scimesh rendering backend: `vignette("fsbrain_with_scimesh")` or: [read online](https://dfsp-spirit.github.io/fsbrain/articles/fsbrain_with_scimesh.html) (also on [CRAN](https://cran.r-project.org/web/packages/fsbrain/vignettes/fsbrain_with_scimesh.html))

* [Online Notebooks including demo workflows for fMRI result visualization](https://dfsp-spirit.github.io/fsbrain_fMRI_vis_workflows/).

* Help for a specific function can be accessed in the usual R manner: `?<function>`, where you replace `<function>` with a function name. Like this: `?group.morph.native`.
* Run `example(<function>)` to see a live demo that uses the function `<function>`. Like this: `example(group.morph.native)`.
* The [unit tests](./tests/testthat/) that come with this package are essentially a list of examples that illustrate how to use the functions.



## License

The *fsbrain* package is [free software](https://en.wikipedia.org/wiki/Free_software), published under the [MIT license](https://opensource.org/licenses/MIT).

Note: The file LICENSE in this repository is a CRAN license template only (as required by CRAN) and does not contain the full MIT  license text. See the file [LICENSE_FULL](./LICENSE_FULL) for the full license text.


## Citation and Publications

You can generate the citation for [our fsbrain paper](https://doi.org/10.1101/2020.09.18.302935) by typing the following command in R:

```
citation("fsbrain")
```

This currently outputs:

```
To cite fsbrain in publications use:

  Tim Schaefer, Christine Ecker (2020). fsbrain: an R package for the visualization of structural neuroimaging data. bioRxiv doi: 10.1101/2020.09.18.302935

A BibTeX entry for LaTeX users is

  @Misc{,
    title = {fsbrain: an {R} package for the visualization of structural neuroimaging data},
    author = {Tim Schaefer and Christine Ecker},
    year = {2020},
    url = {https://www.biorxiv.org/content/10.1101/2020.09.18.302935v1},
    doi = {10.1101/2020.09.18.302935},
  }
```

Other materials related to fsbrain:

* A poster on *fsbrain* has been presented at INSAR 2020 Annual Meeting: [Abstract](https://insar.confex.com/insar/2020/meetingapp.cgi/Paper/33181), [ePoster viewer](https://insar.confex.com/insar/2020/techdemo/eposter.cgi?eposterid=227), [PDF download](https://github.com/dfsp-spirit/fsbrain_gallery/raw/master/extra_materials/Poster_IMFAR2020_fsbrain.pdf)

## Visualization examples

The *fsbrain* package support visualizations of different data, and all data can be displayed in one or more views. The figure below shows some examples for surface-based data:

![Visoverview](./web/fsbrain_vis_overview.jpg?raw=true "Some visualization options from fsbrain")
**Fig.2**: *Example output for the fsbrain interactive visualization functions*.

* **Subfigure A** shows the visualization of raw morphometry data (cortical thickness) from native space on the white surface of a subject. The view shows the data in tiles from 8 different angles.
* **Subfigure B** illustrates arbitrary data (p-values in this case) visualized on the regions of the Desikan atlas, using the surface of the fsaverage (standard space template) subject from FreeSurfer. The view shows the data in tiles from 4 different angles.
* **Subfigure C** displays the regions of the Desikan atlas on the white surface of a subject. The colors were loaded from the respective annotation file. The view shows the data in tiles from 4 different angles.

*What* is displayed (morphometry data, atlas regions, arbitrary other data), on *which surface* it is displayed, and *how* it is displayed (a single interactive view, 4 tiles, 9 tiles) is independent and can be selected as needed in fsbrain.

Here is a second figure, showing the same data (the [mean curvature](https://en.wikipedia.org/wiki/Mean_curvature) at each vertex) displayed on 3 different surfaces of a subject: **A** white surface, **B** pial surface, **C** inflated surface.
![Vissurfaces](./web/fsbrain_curvature_surfaces.jpg?raw=true "Curvature visualization on different surfaces, rendered with fsbrain")


The next figure illustrates some options to visualize your results with different backgrounds. **A** Clusters on the white fsaverage surface with sulc background. **B** Region-wise p-values with curv background, inflated fsaverage surface. **C** A background color layer displaying outlines of aparc atlas regions in the respective colors, inflated demo subject surface.

![Visres](./web/fsbrain_vis_bg.jpg?raw=true "Visualization of results and background layers, rendered with fsbrain")


### Animations and videos

Want to see brains spin? [Check this out.](./web/fsbrain_movies.md) (WARNING: loads 8 MB webpage with animated gif).


### Live visualization on the web

You can also use fsbrain in a shiny app, see the [demo code here](./web/shiny_demo/) and a live demo here on posit cloud: [fsbrain in shiny app](https://019f40eb-f687-4aaa-6413-4f7b18f1b292.share.connect.posit.cloud/).

### Volume visualization

Volume visualization is not the main goal of fsbrain, but standard lightbox views and simple 3D views are supported. Have a look at the vignettes or the documentation for the `volvis.lb` function. You can find some [example output here](./web/fsbrain_volume.md).

A particularly useful feature for quality assessment (QA) of FreeSurfer reconstructions is `volvis.lb.with.surface()`, which overlays the cortical surface boundary contours onto 2D MRI slices — similar to what `freeview` shows:

![Visvoloutline](./web/fsbrain_vol_outline.png?raw=true "Volume slizes with contours of white and pial surfaces, shown in red and yellow respectively. Rendered with fsbrain")



### Example Notebooks

To see a combination of example figures and the code used to produce them, you should have a look at the example notebooks: [getting started notebook](https://htmlpreview.github.io/?https://github.com/dfsp-spirit/fsbrain/blob/develop/web/Rmd_web_examples/examples.html) and [advanced examples notebook](https://htmlpreview.github.io/?https://github.com/dfsp-spirit/fsbrain/blob/develop/web/Rmd_web_examples/examples_adv.html).

## Contributing

Please refer to [CONTRIBUTING.md](./CONTRIBUTING.md). For dev setup instructions, also read [README_DEVELOPMENT.md](./README_DEVELOPMENT.md).

If you have any question, suggestion or comment on fsbrain, please [open an issue](https://github.com/dfsp-spirit/fsbrain/issues). If you want to contact me via email, please use the maintainer email address listed on the [CRAN webpage for fsbrain](https://cran.r-project.org/package=fsbrain).


## Related R packages

Packages similar to fsbrain:

* [ggseg](https://github.com/LCBC-UiO/ggseg) by Athanasia Mowinckel and Didac Vidal-Piñeiro: Plotting of atlas-based neuroimaging data in R.
* [cerebroviz](https://github.com/ethanbahl/cerebroViz) by Ethan Bahl: Data mapping tool for visualizing spatiotemporal data in the brain.

Packages used by fsbrain:

* [scimesh](https://CRAN.R-project.org/package=scimesh) by Tim Schäfer: Headless C++ software renderer for 3D meshes. No GPU or X11 required.
* [rgl](https://CRAN.R-project.org/package=rgl) by Daniel Adler, Duncan Murdoch et al.: OpenGL-based mesh renderer.
* [oro.nifti](https://github.com/muschellij2/oro.nifti) by Brandon Witcher et al. : Loading and manipulation of brain volumes from NIFTI v1 files.
* [freesurferformats](https://github.com/dfsp-spirit/freesurferformats) by Tim Schäfer (me): Loading and writing various neuroimaging file formats and general mesh file formats, with a focus on FreeSurfer formats.
* [gifti](https://github.com/muschellij2/gifti/) and [cifti](https://github.com/muschellij2/cifti/) by John Muschelli: Read GIFTI and CIFTI format files.
* [Rvcg](https://github.com/zarquon42b/Rvcg) by Stefan Schlager: Rcpp interface for the [VCG Library](http://vcg.isti.cnr.it/vcglib/).


## Author

fsbrain was written by [Tim Schäfer](https://ts.rcmd.org)
