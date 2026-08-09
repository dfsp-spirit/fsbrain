# Get subject names from sub directories of FreeSurfer long directory.

Find all subject names for which the FreeSurfer longitudinal pipeline
may have finished. These are the subjects that have the `_MR1` and
`_MR2` directories.

## Usage

``` r
fslong.subjects.detect(subjects_dir, timepoint_names = c("_MR1", "_MR2"))
```

## Arguments

- subjects_dir:

  character string, path to a single recon-all longitudinal output dir
  from FreeSurfer.
