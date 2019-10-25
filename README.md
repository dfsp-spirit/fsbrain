# fsbrain
A GNU R library for structural neuroimaging. Provides high-level functions to access (read and write) and visualize surface-based brain morphometry data (e.g. cortical thickness) for individual subjects and groups.

![Vis](./vignettes/rgl_brain_ct.jpg?raw=true "Cortical thickness visualization")


## Installation

From an R session:

```r
install.packages(c("devtools", "knitr", "testthat"));
devtools::install_github("dfsp-spirit/fsbrain", build_vignettes=TRUE);
```

## Documentation

The documentation comes with the package, and includes the built-in help, examples, and a vignette that explains typical workflows.


## Unit tests and CI

The unit tests are run on Continuous Integration for both Linux and Windows:

Travis (Linux):  [![Build Status](https://travis-ci.org/dfsp-spirit/fsbrain.svg?branch=master)](https://travis-ci.org/dfsp-spirit/fsbrain)

AppVeyor (Windows): [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/dfsp-spirit/fsbrain?branch=master&svg=true)](https://ci.appveyor.com/project/dfsp-spirit/fsbrain)

## License

MIT


## Citation

You can generate the citation for the version you use by typing the following command in R:

```
citation("fsbrain")
```

This will ouput something like this (but for the version you actually used):
```
To cite package ‘fsbrain’ in publications use:

  Tim Schäfer (2019). fsbrain: Managing and Visualizing Brain Surface Data. R package version
  0.0.1. https://github.com/dfsp-spirit/fsbrain

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {fsbrain: Managing and Visualizing Brain Surface Data},
    author = {Tim Schäfer},
    year = {2019},
    note = {R package version 0.0.1},
    url = {https://github.com/dfsp-spirit/fsbrain},
  }
```
