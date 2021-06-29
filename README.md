# fsbrain
A GNU R library for structural neuroimaging. Provides high-level functions to access (read and write) and visualize surface-based brain morphometry data (e.g. cortical thickness) for individual subjects and groups.

![Vis](https://github.com/dfsp-spirit/fsbrain_gallery/blob/master/surface/fsbrain_sulcal_depth_cbar_web.jpg?raw=true "Sulcal depth visualization, created with fsbrain")

**Fig.1**: *Visualization of sulcal depth for a subject in FreeSurfer standard space (fsaverage). See the [source code to reproduce this image](https://htmlpreview.github.io/?https://github.com/dfsp-spirit/fsbrain/blob/develop/web/Rmd_web_examples/examples_export.html) in an R notebook.*

[About](#about) | [Installation](#installation) | [Documentation](#documentation) | [Unit tests](#unit-tests-and-continuous-integration) | [License](#license) | [Citation](#citation) | [Visualization examples](#visualization-examples) | [Contributing](#contributing)


## About

The *fsbrain* R package provides a well-tested and consistent interface to neuroimaging data in [R](https://www.r-project.org/). It supports reading, writing, and visualizing various kinds of raw data and statistical results on brain surfaces and volumes. While the package provides a very convenient interface for working with data arranged in the standard [FreeSurfer](http://freesurfer.net/) directory structure (SUBJECTS_DIR), *fsbrain* is not limited to this layout or FreeSurfer file formats. You can load brain meshes, volumes, and data from a range of other neuroimaging software packages and visualize them.

The plots produced by *fsbrain* can be integrated into R notebooks or written to high-quality bitmap image files, ready for publication. The [rgl](https://CRAN.R-project.org/package=rgl) renderer used by *fsbrain* provides fast, hardware-accelerated rendering based on the OpenGL standard.


## News
* 2021-05-12: New fsbrain version 0.4.3 released on CRAN, see the [CHANGES](./CHANGES).
* 2021-03-28: New fsbrain version 0.4.2 released on CRAN, see the [CHANGES](./CHANGES).
* 2020-09-20: The preprint of our paper [T. Schaefer, C. Ecker: fsbrain: an R package for the visualization of structural neuroimaging data](https://doi.org/10.1101/2020.09.18.302935)' is now available on biorxiv.

## Installation


### Recommended: install the stable fsbrain version from CRAN

You can find the [fsbrain package on CRAN](https://cran.r-project.org/package=fsbrain), so all you need to do is:

```r
install.packages("fsbrain");
```

In case something goes wrong, don't worry. Just install the missing [system dependencies](#system-dependencies) and retry.


### Risky: install the dev version of fsbrain with the latest features

This version is not guaranteed to be in a usable state, try at your own risk and run the tests before using it.

From an R session:

```r
install.packages(c("devtools", "knitr", "markdown", "rmarkdown", "testthat", "qpdf"));
devtools::install_github("dfsp-spirit/fsbrain", build_vignettes=TRUE);
```

### System dependencies

A *system dependency* is a **non-R** software that is needed for the installation of a package. System dependencies cannot be installed automatically using the R package system, so you need to install them manually or using the package manager of your operating system.

The *fsbrain* package itself does not have any system dependencies, however, it uses *rgl* for rendering. You can check the *SystemRequirements* section on the [rgl page at CRAN](https://CRAN.R-project.org/package=rgl) for the full list of rgl dependencies or read on. To get GIFTI format support, you will also need `libxml2-dev`.

To install the system dependencies for *rgl* and *xml2*:

#### Linux System dependencies (or: building from source)

R packages are compiled from source by default under Linux, so you need some development libraries. Before installing *fsbrain*, run the following command in your system shell (not in R):

* for deb-based Linux distributions (Debian, Ubuntu, ...):
```shell
sudo apt-get install libmagick++-dev libx11-dev libgl1-mesa-dev libglu1-mesa-dev mesa-common-dev libfreetype6-dev libxml2-dev libssh-dev libcurl4-openssl-dev libgfortran4
```
* for rpm-based Linux distributions (Fedora, CentOS, RHEL, ...):
```shell
sudo yum install ImageMagick-c++-devel libX11-devel mesa-libGLU-devel freetype-devel libxml2-devel
```

If you want to compile the package under any other operating system, you will need the libraries as well, of course.

#### MacOS System dependencies

Recent MacOS versions do not ship with an X11 environment. You will have to install the [xquartz X11 system](https://www.xquartz.org/) if you do not have it already. If you want to create GIF movies, make sure you have imagemagick installed (easiest via [homebrew](https://brew.sh/): `brew install imagemagick@6`).


### Installation via Docker

The official Docker image for fsbrain 0.4.3 is now available at the [fsbrain Dockerhub repo](https://hub.docker.com/r/dfspspirit/fsbrain). Usage instructions can be found there as well.


## Documentation

The documentation can be accessed from within an R session after you have loaded the *fsbrain* package:

* There are two online R Markdown notebooks (like Jupyter Notebook in Python) that show various example plots in combination with the code used to produce them:
  * [basic fsbrain example notebook](https://htmlpreview.github.io/?https://github.com/dfsp-spirit/fsbrain/blob/develop/web/Rmd_web_examples/examples.html): Live visualization of subject data
  * [advanced fsbrain example notebook](https://htmlpreview.github.io/?https://github.com/dfsp-spirit/fsbrain/blob/develop/web/Rmd_web_examples/examples_adv.html): Plotting group data
  * [export API fsbrain example notebook](https://htmlpreview.github.io/?https://github.com/dfsp-spirit/fsbrain/blob/develop/web/Rmd_web_examples/examples_export.html): Exporting publication-ready plots
* A detailed vignette with explanations and examples for the functions of the package is included, run `browseVignettes("fsbrain")` to see the vignettes. You can also open the vignette directly:
  * learn how to load and visualize surface-based neuroimaging data and results: `vignette("fsbrain")` [read online at CRAN](https://cran.r-project.org/web/packages/fsbrain/vignettes/fsbrain.html)
  * learn how to load and visualize volume-based neuroimaging data and results: `vignette("fsbrain_vol")` [read online at CRAN](https://cran.r-project.org/web/packages/fsbrain/vignettes/fsbrain_vol.html)
  * read the fsbrain FAQ: `vignette("fsbrain_faq")` [read online at CRAN](https://cran.r-project.org/web/packages/fsbrain/vignettes/fsbrain_faq.html)
* Help for a specific function can be accessed in the usual R manner: `?<function>`, where you replace `<function>` with a function name. Like this: `?group.morph.native`.
* Run `example(<function>)` to see a live demo that uses the function `<function>`. Like this: `example(group.morph.native)`.
* The [unit tests](./tests/testthat/) that come with this package are essentially a list of examples that illustrate how to use the functions.



## Unit tests and Continuous integration


This package comes with [lots of unit tests](./tests/testthat/). To run them, in a clean R session:

```r
library(devtools)
library(fsbrain)
devtools::check()
```

Continuous integration results:

<!-- badges: start -->
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/dfsp-spirit/fsbrain?branch=master&svg=true)](https://ci.appveyor.com/project/dfsp-spirit/fsbrain) AppVeyor CI under Windows

[![R-CMD-check](https://github.com/dfsp-spirit/fsbrain/workflows/R-CMD-check/badge.svg)](https://github.com/dfsp-spirit/fsbrain/actions) GitHub Actions, Ubuntu Linux and MacOS (Note: Currently this always fails because of a warning caused by rgl when running headless, so the badge is rather useless atm and one needs to follow the link to see the relevant CI results.)
<!-- badges: end -->

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

### Volume visualization

First versions of some volume visualization functions are available in the current dev version. You can find [a preview of the results here](./web/fsbrain_volume.md).

### Example Notebooks

To see a combination of example figures and the code used to produce them, you should have a look at the example notebooks: [getting started notebook](https://htmlpreview.github.io/?https://github.com/dfsp-spirit/fsbrain/blob/develop/web/Rmd_web_examples/examples.html) and [advanced examples notebook](https://htmlpreview.github.io/?https://github.com/dfsp-spirit/fsbrain/blob/develop/web/Rmd_web_examples/examples_adv.html).

## Contributing

Please refer to [CONTRIBUTING.md](./CONTRIBUTING.md).

If you have any question, suggestion or comment on fsbrain, please [open an issue](https://github.com/dfsp-spirit/fsbrain/issues). If you want to contact me via email, please use the maintainer email address listed on the [CRAN webpage for fsbrain](https://cran.r-project.org/package=fsbrain).


## Related R packages

Packages similar to fsbrain:

* [ggseg](https://github.com/LCBC-UiO/ggseg) by Athanasia Mowinckel and Didac Vidal-Piñeiro: Plotting of atlas-based neuroimaging data in R.
* [cerebroviz](https://github.com/ethanbahl/cerebroViz) by Ethan Bahl: Data mapping tool for visualizing spatiotemporal data in the brain.

Packages used by fsbrain:

* [rgl](https://CRAN.R-project.org/package=rgl) by Daniel Adler, Duncan Murdoch et al.: OpenGL-based mesh renderer.
* [oro.nifti](https://github.com/muschellij2/oro.nifti) by Brandon Witcher et al. : Loading and manipulation of brain volumes from NIFTI v1 files.
* [freesurferformats](https://github.com/dfsp-spirit/freesurferformats) by Tim Schäfer (me): Loading and writing various neuroimaging file formats and general mesh file formats, with a focus on FreeSurfer formats.
* [gifti](https://github.com/muschellij2/gifti/) and [cifti](https://github.com/muschellij2/cifti/) by John Muschelli: Read GIFTI and CIFTI format files.
* [Rvcg](https://github.com/zarquon42b/Rvcg) by Stefan Schlager: Rcpp interface for the [VCG Library](http://vcg.isti.cnr.it/vcglib/).
