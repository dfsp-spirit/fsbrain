# fsbrain
A GNU R library for structural neuroimaging. Provides high-level functions to access (read and write) and visualize surface-based brain morphometry data (e.g. cortical thickness) for individual subjects and groups.

![Vis](./vignettes/fsbrain_ct.jpg?raw=true "Cortical thickness visualization, created with fsbrain")


[Installation](#installation) | [Documentation](#documentation) | [Unit tests](#unit-tests-and-continuous-integration) | [License](#license) | [Citation](#citation) | [Visualization examples](#visualization-examples) | [Contributing](#contributing)

## Installation

### Recommended: install the stable fsbrain version from CRAN

You can find the [fsbrain package on CRAN](https://cran.r-project.org/package=fsbrain), so all you need to do is:

```r
install.packages("fsbrain");
```

### Risky: install the dev version of fsbrain with the latest features

This version is not guaranteed to be in a usable state, try at your own risk and run the tests before using it.

From an R session:

```r
install.packages(c("devtools", "knitr", "rmarkdown", "testthat"));
devtools::install_github("dfsp-spirit/fsbrain", build_vignettes=TRUE);
```

### System dependencies

The *fsbrain* package itself does not have any system dependencies, however, it uses rgl for rendering. You can check the *SystemRequirements* section on the [rgl page at CRAN](https://CRAN.R-project.org/package=rgl) for the full list of rgl dependencies.

To install the system dependencies for rgl:

#### Linux System dependencies

R packages are compiled under Linux, so you need some dev libraries. Before installing *fsbrain*, run the following command in your system shell (not in R):

deb-based distros (Debian, Ubuntu, ...):
```shell
sudo apt-get install libmagick++-dev libx11-dev libglu1-mesa-dev mesa-common-dev libfreetype6-dev
```
rpm-based distros (Fedora, CentOS, RHEL, ...):
```shell
sudo yum install ImageMagick-c++-devel libX11-devel mesa-libGLU-devel freetype-devel
```

#### MacOS System dependencies

Recent MacOS versions do not ship with an X11 environment. You will have to install the [xquartz X11 system](https://www.xquartz.org/) if you do not have it already. If you want to create GIF movies, make sure you have imagemagick installed (easiest via [homebrew](https://brew.sh/): `brew install imagemagick@6`).


## Documentation

The documentation can be accessed from within an R session after you have loaded the *fsbrain* package:

* A detailed vignette with explanations and examples for the functions of the package is included, run `browseVignettes("fsbrain")` to see the vignettes. You can also open the vignette directly:
  * learn how to load and visualize neuroimaging data and results: `vignette("fsbrain")` [read online at CRAN](https://cran.r-project.org/web/packages/fsbrain/vignettes/fsbrain.html)  
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

[![Build Status](https://travis-ci.org/dfsp-spirit/fsbrain.svg?branch=master)](https://travis-ci.org/dfsp-spirit/fsbrain) Travis CI under Linux

[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/dfsp-spirit/fsbrain?branch=master&svg=true)](https://ci.appveyor.com/project/dfsp-spirit/fsbrain) AppVeyor CI under Windows

The displayed status represents the development version. Don't worry if you are using the stable version from CRAN and CI is currently failing, development happens on master.


## License

The *fsbrain* package is [free software](https://en.wikipedia.org/wiki/Free_software), published under the [MIT license](https://opensource.org/licenses/MIT).

Note: The file LICENSE in this repository is a CRAN license template only (as required by CRAN) and does not contain the full MIT  license text. See the file [LICENSE_FULL](./LICENSE_FULL) for the full license text.


## Citation

You can generate the citation for the version you use by typing the following command in R:

```
citation("fsbrain")
```

This will ouput something like this (but for the version you actually used, which is important for reproducibility):
```

To cite package ‘fsbrain’ in a publications use:

  Tim Schäfer (2019). fsbrain: Managing and Visualizing Brain Surface Data. R package version
  0.0.3. https://CRAN.R-project.org/package=fsbrain


A BibTeX entry for LaTeX users is


  @Manual{,
    title = {fsbrain: Managing and Visualizing Brain Surface Data},
    author = {Tim Schäfer},
    year = {2019},
    note = {R package version 0.0.3},
    url = {https://CRAN.R-project.org/package=fsbrain},
    doi = {10.5281/zenodo.3559816},
  }
```

The Digital Object Identifier (DOI) for freesurferformats is: [10.5281/zenodo.3559816](https://dx.doi.org/10.5281/zenodo.3559816)

Note that this DOI always points to the latest version, so be sure to still include the package version in the citation.



## Visualization examples

The *fsbrain* package support visualizations of different data, and all data can be displayed in one or more views. The figure below shows some examples:

![Visoverview](./web/fsbrain_vis_overview.jpg?raw=true "Some visualization options from fsbrain")

* **Subfigure A** shows the visualization of raw morphometry data (cortical thickness) from native space on the white surface of a subject. The view shows the data in tiles from 8 different angles.
* **Subfigure B** illustrates arbitrary data (p-values in this case) visualized on the regions of the Desikan atlas, using the surface of the fsaverage (standard space template) subject from FreeSurfer. The view shows the data in tiles from 4 different angles.
* **Subfigure C** displays the regions of the Desikan atlas on the white surface of a subject. The colors were loaded from the respective annotation file. The view shows the data in tiles from 4 different angles.

*What* is displayed (morphometry data, atlas regions, arbitray other data), on *which surface* it is displayed, and *how* it is displayed (a single interactive view, 4 tiles, 9 tiles) is independent and can be selected as needed in fsbrain.

Here is a second figure, showing the same data (the mean curvature at each vertex) displayed on 3 different surfaces of a subject: **A** white surface, **B** pial surface, **C** inflated surface.

![Vissurfaces](./web/fsbrain_curvature_surfaces.jpg?raw=true "Curvature visualization on different surfaces, rendered with fsbrain")

### Animations and videos

Want to see brains spin? [Check this out.](./web/fsbrain_movies.md) (WARNING: loads 8 MB webpage with animated gif).


## Contributing

Please refer to [CONTRIBUTING.md](./CONTRIBUTING.md).

If you have any questions or want to contact me, please [open an issue](https://github.com/dfsp-spirit/fsbrain/issues).
