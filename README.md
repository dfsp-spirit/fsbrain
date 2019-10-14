# nitools 
A GNU R library for structural neuroimaging. Provides high-level functions to access (read and write) and visualize surface-based brain morphometry data (e.g. cortical thickness) for individual subjects and groups.


## Installation

From an R session:

```r
install.packages(c("devtools", "knitr", "testthat"));
devtools::install_github("dfsp-spirit/nitools", build_vignettes=TRUE);
```

## Documentation

The documentation comes with the package, and includes the built-in help, examples, and a vignette that explains typical workflows.


## Unit tests and CI

The unit tests are run on Continuous Integration for both Linux and Windows:

Travis (Linux):  [![Build Status](https://travis-ci.org/dfsp-spirit/nitools.svg?branch=master)](https://travis-ci.org/dfsp-spirit/nitools)

AppVeyor (Windows): [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/dfsp-spirit/nitools?branch=master&svg=true)](https://ci.appveyor.com/project/dfsp-spirit/nitools)
