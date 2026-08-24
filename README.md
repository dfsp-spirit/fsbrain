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
* 2026-07-09: New fsbrain version 0.6.1 released. Adds automatic fallback for plot export on recent macOS versions (Tahoe, Sonoma) where X11/XQuartz is broken. You can now export publication-ready plots with colorbars even without a working X11 display. See [README_HEADLESS.md](./README_HEADLESS.md) for details.
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

The fsbrain package comes with some optional features. If you want all features:

```r
install.packages("fsbrain", dependencies=TRUE);
```

If you are using a platform that defaults to building fsbrain from source, like Linux, and you are getting errors during installation about missing system dependencies, do not worry: just read [INSTALL_FSBRAIN_FROM_SOURCE.md](./INSTALL_FSBRAIN_FROM_SOURCE.md) for instructions.


### Headless / no-display rendering: the scimesh backend

If you work headless (HPC clusters, servers, CI runners, containers) or on recent macOS where XQuartz is broken, fsbrain's default rgl backend cannot open windows. Switch to the [scimesh](https://CRAN.R-project.org/package=scimesh) software renderer — a headless, GPU-free C++ renderer that produces identical static images without X11, OpenGL, or a GPU:

```r
install.packages("scimesh");
options(fsbrain.renderer_backend = "scimesh");
```

Static image export (`vislayout.from.coloredmeshes()` and `export()`) now renders with scimesh. Interactive views (e.g., `views = "si"`, `"sr"`, `"t4"`, `"t9"`) and `vis.rglwidget()` still use rgl. Switch back at any time with `options(fsbrain.renderer_backend = "rgl")`.

* **How to use it**: see the [scimesh vignette](https://dfsp-spirit.github.io/fsbrain/articles/fsbrain_with_scimesh.html) (`vignette("fsbrain_with_scimesh")`) — what works, the limitations, and many worked examples.
* **Why / when to use it, and alternatives**: see [README_HEADLESS.md](./README_HEADLESS.md), a decision guide for headless environments and broken-X11 macOS.


#### Windows Installation Hints

We received reports that under Windows 10, you may need to install these two packages manually via the `install.packages` command: `shiny` and `manipulateWidget`.



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
