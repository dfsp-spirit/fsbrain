## R CMD check results on local R CMD check --as-cran
There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
  Found the following (possibly) invalid URLs:
    URL: https://www.biorxiv.org/content/10.1101/2020.09.18.302935v1
    From: inst/CITATION
    Status: 403
    Message: Forbidden

### Notes on False Positives
* The bioRxiv URL in `inst/CITATION` is valid and resolves correctly in a standard browser. The 403 Forbidden status is maybe due to bioRxiv's automated protection blocking the R CMD check scraper.


### Full local output




## Winbuilder

We also check on Winbuilder.