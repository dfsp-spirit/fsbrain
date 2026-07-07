# fsbrain development information


See [CONTRIBUTING.md](./CONTRIBUTING.md) for contribution guidelines and general workflow.

## Recommended dev environment

* clone the git repo
* install rstudio and R if you do not have them yet
* install the following R packages: `devtools, knitr, testthat`
* install system dependencies of fsbrain, as explained in the installation instruction in [README.md](README.md).
* install all fsbrain dependencies, e.g., by installing fsbrain from CRAN using `install.packages("fsbrain", dependencies=TRUE);`
* in rstudio, click `File => Open Project` and open the project file `fsbrain.Rproj` from the root of this repo

## Running the unit tests

In rstudio, click *Build - Test Package*.

On the console:

* to run all tests based on source code in dir: ```Rscript -e "devtools::test()"```
* to run an individual test, or several ones, by name filter: ```Rscript -e "devtools::test(filter = 'morph_agg')"```

## Checking the package

This does a lot more than just running the tests, it checks various coding styles, metadata, and all kinds of other stuff that is specific to what the people running CRAN want you to do. It also builds the documentation by default to check whether that works, so it takes a lot of time.

In rstudio, click *Build - Check Package*.

On the console:

* to run CRAN checks on source: ```Rscript -e "devtools::check()"```
* to build package and run CRAN checks on build version: ```R CMD build . && R CMD check fsbrain_0.5.1.tar.gz```, or whatever version your are building
* to build package and run only package checks and tests (faster): ```R CMD check . --no-manual --no-vignettes```


## Building the documentation (vignettes)

In rstudio, click *Build - Clean and Rebuild*.

On the console, run ```Rscript -e devtools::build_vignettes()```


## Building the function documentation from inline doc strings in the code

You will need to do this if you added a new argument to a function and R CMD check complains about code/documentation mismatches.

On the console, run ```Rscript -e "roxygen2::roxygenise()"```
