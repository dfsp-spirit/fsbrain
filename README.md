# nitools 
My personal GNU R utlity functions for structural neuroimaging research. I doubt many others will want them, but feel free to use them.


## Installation

```r
install.packages(c("devtools", "knitr", "testthat"));
devtools::install_github("dfsp-spirit/nitools", build_vignettes=TRUE);
```

## Unit tests and CI

Travis (Linux):  [![Build Status](https://travis-ci.org/dfsp-spirit/nitools.svg?branch=master)](https://travis-ci.org/dfsp-spirit/nitools)

AppVeyor (Windows): [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/dfsp-spirit/nitools?branch=master&svg=true)](https://ci.appveyor.com/project/dfsp-spirit/nitools)
