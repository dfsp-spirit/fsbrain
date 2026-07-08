## R CMD check results on local R CMD check --as-cran

There was 1 NOTE, but read on:

* checking CRAN incoming feasibility ... NOTE
  Found the following (possibly) invalid URLs:
    URL: https://www.biorxiv.org/content/10.1101/2020.09.18.302935v1
    From: inst/CITATION
    Status: 403
    Message: Forbidden

### Notes on False Positives

* The bioRxiv URL in `inst/CITATION` is valid and resolves correctly in a standard browser. It is the URL to our preprint paper describing this package, and it is important for us to include this.
* The 403 Forbidden status is maybe due to some automated protection on bioRxiv blocking the R CMD check scraper?


### Full local output

```shell
$ R CMD build . && R CMD check --as-cran fsbrain_0.6.0.tar.gz
* checking for file ‘./DESCRIPTION’ ... OK
* preparing ‘fsbrain’:
* checking DESCRIPTION meta-information ... OK
* installing the package to build vignettes
* creating vignettes ... OK
* checking for LF line-endings in source and make files and shell scripts
* checking for empty or unneeded directories
Removed empty directory ‘fsbrain/docker’
Removed empty directory ‘fsbrain/tests/testthat/_snaps’
Removed empty directory ‘fsbrain/web’
* building ‘fsbrain_0.6.0.tar.gz’

* using log directory ‘/home/ts/develop/fsbrain/fsbrain.Rcheck’
* using R version 4.3.3 (2024-02-29)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu3) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu3) 13.2.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--as-cran’
* checking for file ‘fsbrain/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘fsbrain’ version ‘0.6.0’
* package encoding: UTF-8
* checking CRAN incoming feasibility ... [4s/14s] NOTE
Maintainer: ‘Tim Schäfer <ts+code@rcmd.org>’

Found the following (possibly) invalid URLs:
  URL: https://www.biorxiv.org/content/10.1101/2020.09.18.302935v1
    From: inst/CITATION
    Status: 403
    Message: Forbidden
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking serialization versions ... OK
* checking whether package ‘fsbrain’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking for future file timestamps ... NOTE
unable to verify current time
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking use of S3 registration ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd line widths ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ...
  Running ‘testthat_au.R’ [8s/144s]
  Running ‘testthat_vz.R’
 [12s/152s] OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking re-building of vignette outputs ... OK
* checking PDF version of manual ... OK
* checking HTML version of manual ... OK
* checking for non-standard things in the check directory ... OK
* checking for detritus in the temp directory ... OK
* DONE

Status: 2 NOTEs
See
  ‘/home/ts/develop/fsbrain/fsbrain.Rcheck/00check.log’
for details.
```




## Winbuilder

We also check on Winbuilder.

* Status: OK for R version 4.5.3 (2026-03-11 ucrt)
* Status: OK for R version 4.6.1 (2026-06-24 ucrt)
* R Under development (unstable) (2026-07-07 r90210 ucrt) shows 1 error, but it seems to be a bug on CRAN, I think it cached an older version of the package that file I uploaded that had a broken test. I removed the test and re-uploaded the package several times, but the error keeps showing up. The logs are here: https://win-builder.r-project.org/0HOv798EgV96/00check.log and the error is:
*
* "══ Failed tests
   ── Error ('test-issue28-debug.R:446:5'): STEP 8: Compare coloredmeshes — direct annot.outline vs as background
   ..."
   but the file 'test-issue28-debug.R' is nowhere in the repo or in the archive I uploaded.