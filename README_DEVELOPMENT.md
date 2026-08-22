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

## Running the unit tests with the scimesh renderer backend

fsbrain can produce static images with two backends: **rgl** (the default; interactive, hardware-accelerated, needs a display / X11) and **scimesh** (headless, software, no display). The test suite can be run under either backend.

### Selecting the backend

Set the `FSBRAIN_TESTS_USE_SCIMESH` environment variable to a truthy value (`1`, `true`, `yes`, `on`) to run the tests with the scimesh backend:

```sh
FSBRAIN_TESTS_USE_SCIMESH=true Rscript -e 'devtools::test()'
```

or, from an R session:

```r
Sys.setenv("FSBRAIN_TESTS_USE_SCIMESH" = "true");
devtools::test();
```

When the variable is unset (or falsy), the default rgl backend is used.

### What changes in scimesh mode

* Static image export tests (`vislayout.from.coloredmeshes()`, `export()`) render through scimesh, headlessly — no X11 required.
* Tests that require the interactive rgl backend (multi-view windows `t4`/`t9`/`si`/`sr`, highlight spheres, volume contours via `misc3d`, `rglwidget`, rotating views) are skipped automatically via the `skip_if_rgl_required()` helper.
* The test helper `box.has.x11display()` returns `TRUE` in scimesh mode, so X11-gated tests do not require a display.

### Comparing the two backends

The `render.demo(coloredmeshes, name = "demo")` helper renders the given coloredmeshes from the two medial views (`t2`) and writes the image to the R session temporary directory as `<name>_<backend>.png` (e.g. `demo_rgl.png` / `demo_scimesh.png`), printing the full path. Running the suite once per backend lets you open the two images side by side in an image viewer to compare them directly.

### Extra-long / full-data tests

Some tests require the full FreeSurfer `subject1` recon-all output (see `box.can.run.all.tests()` and `testdatapath.subjectsdir.full.subject1()` in `tests/testthat/helper-functions.R`) and are only run when `RUN_ALL_FSBRAIN_TESTS` is set:

```sh
RUN_ALL_FSBRAIN_TESTS=sure devtools::test()
```

### `devtools::test()` vs `R CMD check`

* `devtools::test()` sets `NOT_CRAN=true`, so tests guarded by `testthat::skip_on_cran()` actually run.
* `R CMD check` runs the tests in a CRAN-like mode and **skips** all `skip_on_cran()` tests, which is why the check is much faster and only exercises the lightweight (cube-based) tests.

## Checking the package

This does a lot more than just running the tests, it checks various coding styles, metadata, and all kinds of other stuff that is specific to what the people running CRAN want you to do. It also builds the documentation by default to check whether that works, so it takes a lot of time.

In rstudio, click *Build - Check Package*.

On the console:

* to run CRAN checks on source: ```Rscript -e "devtools::check()"```
* to build package and run CRAN checks on build version: ```R CMD build . && R CMD check fsbrain_0.5.1.tar.gz```, or whatever version your are building
* to build package and run only package checks and tests (faster): ```R CMD check . --no-manual --no-vignettes```
* run the hard-core way before a release, so you do not get bothered by CRAN later: ```R CMD check --as-cran```
*
Observe the output of those check commands carefully, they skip checks if a tool is not installed locally. E.g., to get all checks, you may need to install these:

```sudo apt install pqdf tidy```

## Building the documentation (vignettes)

In rstudio, click *Build - Clean and Rebuild*.

On the console, run ```Rscript -e devtools::build_vignettes()```


## Building the function documentation from inline doc strings in the code

You will need to do this if you added a new argument to a function and R CMD check complains about code/documentation mismatches.

On the console, run ```Rscript -e "roxygen2::roxygenise()"```


## Making a new release

- Make sure all changes are logged in CHANGES file
- Bump version in DESCRIPTION
- Build package and make sure it passes CRAN tests locally. Best done with a recent R version, as they may have introduced even more annoying checks in later versions: ```R CMD check build . && R CMD check --as-cran fsbrain_0.5.0.tar.gz```, or whatever version your are building
- Upload the package to [winbuilder](https://win-builder.r-project.org/upload.aspx) to check there. The service will read package metadata for your email and report back via mail when done.
- If everything is green both locally and on Winbuilder, submit to CRAN via their [package submission form](https://cran.r-project.org/submit.html)
- You will receive feedback from CRAN, either package was accepted or some version of R they test with some check still failed. Bad luck. You will have to modify source and do the loop again.
- Once it passes and CRAN confirms it's on its way to the repo, tag the final git submit that made it into CRAN with the version, e.g. ```git tag v0.5.0 c2hf5hjdk3``` if `c2hf5hjdk3` is the commit ID. Check ```git log --oneline``` for commit IDs. When you have tagged it like this locally, make sure to push the tag: ```git push --tags```.
- Log into github.com, and make a release there based on the tag. Copy relevant CHANGES section as description.